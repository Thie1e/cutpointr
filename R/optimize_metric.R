optimize_metric <- function(data, x, class, metric_func = youden,
                            pos_class = NULL, neg_class = NULL, minmax,
                            direction, metric_name = "metric", loess = FALSE,
                            spline = FALSE, gam = FALSE, return_roc = TRUE,
                            tol_metric, use_midpoints, ...) {
    args <- list(...)
    metric_name_call <- as.character(substitute(metric_func))
    if (metric_name_call != "metric_func") metric_name <- metric_name_call
    roccurve <- roc(data = data, x = x, class = class, pos_class = pos_class,
                    neg_class = neg_class, direction = direction)
    m <- metric_func(tp = roccurve[, "tp"], fp = roccurve[, "fp"],
                     tn = roccurve[, "tn"], fn = roccurve[, "fn"], ...)
    m <- sanitize_metric(m, m_name = metric_name, n = nrow(roccurve))
    roccurve$m <- as.numeric(m)
    if (!is.null(colnames(m))) metric_name <- colnames(m)
    if (loess) {
        roccurve$m_unsmoothed <- roccurve$m
        finite_x <- is.finite(roccurve$x.sorted)
        finite_m <- is.finite(roccurve$m)
        finite_roc <- (finite_x + finite_m) == 2
        mod <- fANCOVA::loess.as(x = roccurve$x.sorted[finite_roc],
                        y = roccurve$m[finite_roc],
                        criterion = args$criterion, degree = args$degree,
                        family = args$family, user.span = args$user.span)
        roccurve$m <- NA
        roccurve$m[finite_roc] <- mod$fitted
    }
    if (spline) {
        roccurve$m_unsmoothed <- roccurve$m
        finite_x <- is.finite(roccurve$x.sorted)
        finite_m <- is.finite(roccurve$m)
        finite_roc <- (finite_x + finite_m) == 2
        if (is.function(args$nknots)) {
            nknots <- args$nknots(data = data, x = x)
            message(paste("nknots:", nknots))
        } else if (is.numeric(args$nknots)) {
            nknots <- args$nknots
        }
        if (!is.null(args[["df"]])) {
            mod <- stats::smooth.spline(x = roccurve$x.sorted[finite_roc],
                                        y = roccurve$m[finite_roc],
                                        w = args[["w"]], spar = args[["spar"]],
                                        cv = FALSE, df = args[["df"]],
                                        nknots = nknots, df.offset = args[["df_offset"]],
                                        penalty = args[["penalty"]],
                                        control.spar = args[["control_spar"]],
                                        tol = 1e-100)
        } else {
            mod <- stats::smooth.spline(x = roccurve$x.sorted[finite_roc],
                                        y = roccurve$m[finite_roc],
                                        w = args[["w"]], spar = args[["spar"]],
                                        cv = FALSE,
                                        nknots = nknots, df.offset = args[["df_offset"]],
                                        penalty = args[["penalty"]],
                                        control.spar = args[["control_spar"]],
                                        tol = 1e-100)
        }
        # smooth.spline builds bins of x values that are closer to each other
        # than tol, so not all original x values may be returned
        if (length(mod$x) != sum(finite_roc)) {
            stop("Not all original x values returned by smooth.spline")
        }
        roccurve$m <- NA
        roccurve$m[finite_roc] <- rev(mod$y)
    }
    if (gam) {
        roccurve$m_unsmoothed <- roccurve$m
        finite_x <- is.finite(roccurve$x.sorted)
        finite_m <- is.finite(roccurve$m)
        finite_roc <- (finite_x + finite_m) == 2
        mod <- mgcv::gam(stats::as.formula(paste(paste0(args$formula[2], " ~"),
                                                 args$formula[3])),
                         data = roccurve[finite_roc, ],
                         optimizer = args[["optimizer"]])
        roccurve$m <- NA
        roccurve$m[finite_roc] <- mod$fitted.values
    }
    if (minmax == "max") {
        m_vals <- stats::na.omit(roccurve$m)
        if (length(m_vals) == 0) stop("All metric values are missing.")
        # max_m <- max(roccurve$m, na.rm = TRUE)
        max_m <- max(m_vals)
        opt <- which((roccurve$m >= (max_m - tol_metric)) &
                         (roccurve$m <= (max_m + tol_metric)))
        oc <- roccurve[, "x.sorted"][opt]
        m_oc <- max_m
    } else if (minmax == "min") {
        m_vals <- stats::na.omit(roccurve$m)
        if (length(m_vals) == 0) stop("All metric values are missing.")
        # min_m <- min(roccurve$m, na.rm = TRUE)
        min_m <- min(m_vals)
        opt <- which((roccurve$m >= (min_m - tol_metric)) &
                         (roccurve$m <= (min_m + tol_metric)))
        oc <- roccurve[, "x.sorted"][opt]
        m_oc <- min_m
    }
    if (length(oc) > 1) {
        res <- tibble::tibble(optimal_cutpoint = list(oc))
        if (use_midpoints) {
            res$optimal_cutpoint <- list(midpoint(oc = unlist(res$optimal_cutpoint),
                                                  x = unlist(data[, x], use.names = FALSE),
                                                  direction = direction))
        }
    } else {
        res <- tibble::tibble(optimal_cutpoint = oc)
        if (use_midpoints) {
            res$optimal_cutpoint <- midpoint(oc = unlist(res$optimal_cutpoint),
                                             x = unlist(data[, x], use.names = FALSE),
                                             direction = direction)
        }
    }
    if (return_roc) {
        res <- dplyr::bind_cols(res, tidyr::nest_(roccurve, key_col = "roc_curve"))
    }
    if (loess) metric_name <- paste0("loess_", metric_name)
    if (spline) metric_name <- paste0("spline_", metric_name)
    if (gam) metric_name <- paste0("gam_", metric_name)
    res[, metric_name] <- m_oc
    return(res)
}

#' Optimize a metric function in binary classification
#'
#' Given a function for computing a metric in \code{metric_func}, these functions
#' maximize or minimize that metric by selecting an optimal cutpoint.
#' The metric function should accept the following inputs:
#' \itemize{
#'  \item \code{tp}: vector of number of true positives
#'  \item \code{fp}: vector of number of false positives
#'  \item \code{tn}: vector of number of true negatives
#'  \item \code{fn}: vector of number of false negatives
#' }
#'
#' The above inputs are arrived at by using all unique values in \code{x}, Inf, or
#' -Inf as possible cutpoints for classifying the variable in class.
#'
#' @return A tibble with the columns \code{optimal_cutpoint}, the corresponding metric
#' value and \code{roc_curve}, a nested tibble that includes all possible cutoffs
#' and the corresponding numbers of true and false positives / negatives and
#' all corresponding metric values.
#'
#' @inheritParams oc_youden_normal
#' @param metric_func (function) A function that computes a
#' metric to be maximized. See description.
#' @param tol_metric All cutpoints will be returned that lead to a metric
#' value in the interval [m_max - tol_metric, m_max + tol_metric] where
#' m_max is the maximum achievable metric value. This can be used to return
#' multiple decent cutpoints and to avoid floating-point problems.
#' @param use_midpoints (logical) If TRUE (default FALSE) the returned optimal
#' cutpoint will be the mean of the optimal cutpoint and the next highest
#' observation (for direction = ">") or the next lowest observation
#' (for direction = "<") which avoids biasing the optimal cutpoint.
#' @param ... Further arguments that will be passed to \code{metric_func}.
#' @examples
#' cutpointr(suicide, dsi, suicide, method = maximize_metric, metric = accuracy)
#' @name maximize_metric
#' @family method functions
#' @export
maximize_metric <- function(data, x, class, metric_func = youden,
                            pos_class = NULL, neg_class = NULL,
                            direction, tol_metric,
                            use_midpoints, ...) {
    metric_name <- as.character(substitute(metric_func))
    optimize_metric(data = data, x = x, class = class, metric_func = metric_func,
                    pos_class = pos_class, neg_class = neg_class, minmax = "max",
                    direction = direction, metric_name = metric_name,
                    tol_metric = tol_metric, use_midpoints = use_midpoints, ...)
}

#' @examples
#' cutpointr(suicide, dsi, suicide, method = minimize_metric, metric = abs_d_sens_spec)
#' @rdname maximize_metric
#' @export
minimize_metric <- function(data, x, class, metric_func = youden,
                            pos_class = NULL, neg_class = NULL,
                            direction, tol_metric, use_midpoints, ...) {
    metric_name <- as.character(substitute(metric_func))
    optimize_metric(data = data, x = x, class = class, metric_func = metric_func,
                    pos_class = pos_class, neg_class = neg_class, minmax = "min",
                    direction = direction, metric_name = metric_name,
                    tol_metric = tol_metric, use_midpoints = use_midpoints, ...)
}

#' Optimize a metric function in binary classification after LOESS smoothing
#'
#' Given a function for computing a metric in \code{metric_func}, these functions
#' smooth the function of metric value per cutpoint using LOESS, then
#' maximize or minimize the metric by selecting an optimal cutpoint. For further details
#' on the LOESS smoothing see \code{?fANCOVA::loess.as}.
#' The \code{metric} function should accept the following inputs:
#' \itemize{
#'  \item \code{tp}: vector of number of true positives
#'  \item \code{fp}: vector of number of false positives
#'  \item \code{tn}: vector of number of true negatives
#'  \item \code{fn}: vector of number of false negatives
#' }
#'
#' The above inputs are arrived at by using all unique values in \code{x}, Inf, and
#' -Inf as possible cutpoints for classifying the variable in class.
#'
#' @return A tibble with the columns \code{optimal_cutpoint}, the corresponding metric
#' value and \code{roc_curve}, a nested tibble that includes all possible cutoffs
#' and the corresponding numbers of true and false positives / negatives and
#' all corresponding metric values.
#'
#' @inheritParams oc_youden_normal
#' @param metric_func (function) A function that computes a
#' metric to be maximized. See description.
#' @param criterion the criterion for automatic smoothing parameter selection:
#'  "aicc" denotes bias-corrected AIC criterion, "gcv" denotes generalized
#'  cross-validation.
#' @param degree the degree of the local polynomials to be used. It can be
#'  0, 1 or 2.
#' @param family if "gaussian" fitting is by least-squares, and if "symmetric"
#'  a re-descending M estimator is used with Tukey's biweight function.
#' @param user.span The user-defined parameter which controls the degree of
#' smoothing
#' @param tol_metric All cutpoints will be returned that lead to a metric
#' value in the interval [m_max - tol_metric, m_max + tol_metric] where
#' m_max is the maximum achievable metric value. This can be used to return
#' multiple decent cutpoints and to avoid floating-point problems.
#' @param use_midpoints (logical) If TRUE (default FALSE) the returned optimal
#' cutpoint will be the mean of the optimal cutpoint and the next highest
#' observation (for direction = ">") or the next lowest observation
#' (for direction = "<") which avoids biasing the optimal cutpoint.
#' @param ... Further arguments that will be passed to metric_func or the
#' loess smoother.
#'
#' @source Xiao-Feng Wang (2010). fANCOVA: Nonparametric Analysis of Covariance.
#'  https://CRAN.R-project.org/package=fANCOVA
#' @source Leeflang, M. M., Moons, K. G., Reitsma, J. B., & Zwinderman, A. H.
#' (2008). Bias in sensitivity and specificity caused by data-driven selection
#' of optimal cutoff values: mechanisms, magnitude, and solutions.
#' Clinical Chemistry, (4), 729–738.
#' @examples
#' oc <- cutpointr(suicide, dsi, suicide, gender, method = maximize_loess_metric,
#' criterion = "aicc", family = "symmetric", degree = 2, user.span = 0.7,
#' metric = accuracy)
#' plot_metric(oc)
#' @name maximize_loess_metric
#' @family method functions
#' @export
maximize_loess_metric <- function(data, x, class, metric_func = youden,
                            pos_class = NULL, neg_class = NULL, direction,
                            criterion = "aicc", degree = 1, family = "symmetric",
                            user.span = NULL, tol_metric, use_midpoints, ...) {
    if (!requireNamespace("fANCOVA", quietly = TRUE)) {
        stop("fANCOVA package has to be installed to use LOESS smoothing.")
    }
    metric_name <- as.character(substitute(metric_func))
    if (is.function(user.span)) {
        us <- user.span(data = data, x = x)
    } else {
        us <- user.span
    }
    optimize_metric(data = data, x = x, class = class, metric_func = metric_func,
                    pos_class = pos_class, neg_class = neg_class, minmax = "max",
                    direction = direction, metric_name = metric_name,
                    criterion = criterion, degree = degree, family = family,
                    user.span = us, loess = TRUE,
                    tol_metric = tol_metric, use_midpoints = use_midpoints, ...)
}

#' @examples
#' oc <- cutpointr(suicide, dsi, suicide, gender, method = minimize_loess_metric,
#' criterion = "aicc", family = "symmetric", degree = 2, user.span = 0.7,
#' metric = misclassification_cost, cost_fp = 1, cost_fn = 10)
#' plot_metric(oc)
#' @rdname maximize_loess_metric
#' @export
minimize_loess_metric <- function(data, x, class, metric_func = youden,
                            pos_class = NULL, neg_class = NULL, direction,
                            criterion = "aicc", degree = 1, family = "symmetric",
                            user.span = NULL, tol_metric, use_midpoints, ...) {
    if (!requireNamespace("fANCOVA", quietly = TRUE)) {
        stop("fANCOVA package has to be installed to use LOESS smoothing.")
    }
    metric_name <- as.character(substitute(metric_func))
    if (is.function(user.span)) {
        us <- user.span(data = data, x = x)
    } else {
        us <- user.span
    }
    optimize_metric(data = data, x = x, class = class, metric_func = metric_func,
                    pos_class = pos_class, neg_class = neg_class, minmax = "min",
                    direction = direction, metric_name = metric_name,
                    criterion = criterion, degree = degree, family = family,
                    loess = TRUE, user.span = us,
                    tol_metric = tol_metric, use_midpoints = use_midpoints, ...)
}

#' Calculate bandwidth for LOESS smoothing of metric functions by rule of thumb
#'
#' This function implements a rule of thumb for selecting the bandwidth when
#' smoothing a function of metric values per cutpoint value, particularly
#' in \code{maximize_loess_metric} and \code{minimize_loess_metric}.
#'
#' The function used for calculating the bandwidth is 0.1 * xsd / sqrt(xn),
#' where xsd is the standard deviation of the unique values of the predictor
#' variable (i.e. all cutpoints) and xn is the number of unique predictor values.
#'
#' @param data A data frame
#' @param x The predictor variable
user_span_cutpointr <- function(data, x) {
    finite_x <- is.finite(data[[x]])
    xsd <- stats::sd(data[[x]][finite_x], na.rm = TRUE)
    xn <- length(unique(data[[x]][finite_x]))

    print(0.1 * xsd / sqrt(xn))
    return(0.1 * xsd / sqrt(xn))
}

#' Optimize a metric function in binary classification after bootstrapping
#'
#' Given a function for computing a metric in \code{metric_func}, these functions
#' bootstrap the data \code{boot_cut} times and
#' maximize or minimize the metric by selecting an optimal cutpoint. The returned
#' optimal cutpoint is the result of applying \code{summary_func}, e.g. the mean,
#' to all optimal cutpoints that were determined in the bootstrap samples.
#' The \code{metric} function should accept the following inputs:
#' \itemize{
#'  \item \code{tp}: vector of number of true positives
#'  \item \code{fp}: vector of number of false positives
#'  \item \code{tn}: vector of number of true negatives
#'  \item \code{fn}: vector of number of false negatives
#' }
#'
#' The above inputs are arrived at by using all unique values in \code{x}, Inf, and
#' -Inf as possible cutpoints for classifying the variable in class.
#' The reported metric represents the usual in-sample performance of the
#' determined cutpoint.
#'
#' @return A tibble with the column \code{optimal_cutpoint}
#'
#' @inheritParams oc_youden_normal
#' @param metric_func (function) A function that computes a single number
#' metric to be maximized. See description.
#' @param summary_func (function) After obtaining the bootstrapped optimal
#' cutpoints this function, e.g. mean or median, is applied to arrive at a single cutpoint.
#' @param boot_cut (numeric) Number of bootstrap repetitions over which the mean
#' optimal cutpoint is calculated.
#' @param inf_rm (logical) whether to remove infinite cutpoints before
#' calculating the summary.
#' @param tol_metric All cutpoints will be passed to \code{summary_func}
#' that lead to a metric
#' value in the interval [m_max - tol_metric, m_max + tol_metric] where
#' m_max is the maximum achievable metric value. This can be used to return
#' multiple decent cutpoints and to avoid floating-point problems.
#' @param use_midpoints (logical) If TRUE (default FALSE) the returned optimal
#' cutpoint will be the mean of the optimal cutpoint and the next highest
#' observation (for direction = ">") or the next lowest observation
#' (for direction = "<") which avoids biasing the optimal cutpoint.
#'
#' @examples
#' set.seed(100)
#' cutpointr(suicide, dsi, suicide, method = maximize_boot_metric,
#'           metric = accuracy, boot_cut = 30)
#' @name maximize_boot_metric
#' @family method functions
#' @export
maximize_boot_metric <- function(data, x, class, metric_func = youden,
                            pos_class = NULL, neg_class = NULL, direction,
                            summary_func = mean, boot_cut = 50, inf_rm = TRUE,
                            tol_metric, use_midpoints, ...) {
    metric_name <- as.character(substitute(metric_func))
    # ind_pos <- which(unlist(data[, class]) == pos_class)
    # ind_neg <- which(unlist(data[, class]) == neg_class)
    optimal_cutpoints <- purrr::map(1:boot_cut, function(i) {
        b_ind <- simple_boot(data = data, dep_var = class)
        opt_cut <- optimize_metric(data = data[b_ind, ],
                                   x = x, class = class,
                                   metric_func = metric_func,
                                   pos_class = pos_class, neg_class = neg_class,
                                   minmax = "max", direction = direction,
                                   metric_name = metric_name, return_roc = FALSE,
                                   tol_metric = tol_metric,
                                   use_midpoints = use_midpoints, ...)
        return(unlist(opt_cut$optimal_cutpoint))
    })
    optimal_cutpoints <- unlist(optimal_cutpoints)
    if (inf_rm) optimal_cutpoints <- optimal_cutpoints[is.finite(optimal_cutpoints)]
    return(data.frame(optimal_cutpoint = summary_func(optimal_cutpoints)))
}

#' @rdname maximize_boot_metric
#' @examples
#' set.seed(100)
#' cutpointr(suicide, dsi, suicide, method = minimize_boot_metric,
#'           metric = abs_d_sens_spec, boot_cut = 30)
#' @export
minimize_boot_metric <- function(data, x, class, metric_func = youden,
                            pos_class = NULL, neg_class = NULL, direction,
                            summary_func = mean, boot_cut = 50, inf_rm = TRUE,
                            tol_metric, use_midpoints, ...) {
    metric_name <- as.character(substitute(metric_func))
    # ind_pos <- which(unlist(data[, class]) == pos_class)
    # ind_neg <- which(unlist(data[, class]) == neg_class)
    optimal_cutpoints <- purrr::map(1:boot_cut, function(i) {
        b_ind <- simple_boot(data = data, dep_var = class)
        opt_cut <- optimize_metric(data = data[b_ind, ],
                                   x = x, class = class,
                                   metric_func = metric_func,
                                   pos_class = pos_class, neg_class = neg_class,
                                   minmax = "min", direction = direction,
                                   metric_name = metric_name, return_roc = FALSE,
                                   tol_metric = tol_metric,
                                   use_midpoints = use_midpoints, ...)
        return(unlist(opt_cut$optimal_cutpoint))
    })
    optimal_cutpoints <- unlist(optimal_cutpoints)
    if (inf_rm) optimal_cutpoints <- optimal_cutpoints[is.finite(optimal_cutpoints)]
    return(data.frame(optimal_cutpoint = summary_func(optimal_cutpoints)))
}



#' Optimize a metric function in binary classification after spline smoothing
#'
#' Given a function for computing a metric in \code{metric_func}, this function
#' smoothes the function of metric value per cutpoint using smoothing splines. Then it
#' optimizes the metric by selecting an optimal cutpoint. For further details
#' on the smoothing spline see \code{?stats::smooth.spline}.
#' The \code{metric} function should accept the following inputs:
#' \itemize{
#'  \item \code{tp}: vector of number of true positives
#'  \item \code{fp}: vector of number of false positives
#'  \item \code{tn}: vector of number of true negatives
#'  \item \code{fn}: vector of number of false negatives
#' }
#'
#' The above inputs are arrived at by using all unique values in \code{x}, Inf, and
#' -Inf as possible cutpoints for classifying the variable in class.
#'
#' @return A tibble with the columns \code{optimal_cutpoint}, the corresponding metric
#' value and \code{roc_curve}, a nested tibble that includes all possible cutoffs
#' and the corresponding numbers of true and false positives / negatives and
#' all corresponding metric values.
#'
#' @inheritParams oc_youden_normal
#' @param metric_func (function) A function that computes a
#' metric to be optimized. See description.
#' @param w Optional vector of weights of the same length as x; defaults to all 1.
#' @param df The desired equivalent number of degrees of freedom
#' (trace of the smoother matrix). Must be in (1,nx], nx the number of
#' unique x values.
#' @param spar Smoothing parameter, typically (but not necessarily) in (0,1].
#' When spar is specified, the coefficient lambda of the integral of the squared
#' second derivative in the fit (penalized log likelihood) criterion is a
#' monotone function of spar.
#' @param nknots Integer or function giving the number of knots. The function
#' should accept data and x (the name of the predictor variable) as inputs.
#' By default nknots = 0.1 * log(n_dat / n_cut) * n_cut where n_dat is the
#' number of observations and n_cut the number of unique predictor values.
#' @param df_offset Allows the degrees of freedom to be increased by df_offset
#' in the GCV criterion.
#' @param penalty The coefficient of the penalty for degrees of freedom in the
#' GCV criterion.
#' @param control_spar Optional list with named components controlling the root
#' finding when the smoothing parameter spar is computed, i.e., NULL. See
#' help("smooth.spline") for further information.
#' @param tol_metric All cutpoints will be returned that lead to a metric
#' value in the interval [m_max - tol_metric, m_max + tol_metric] where
#' m_max is the maximum achievable metric value. This can be used to return
#' multiple decent cutpoints and to avoid floating-point problems.
#' @param use_midpoints (logical) If TRUE (default FALSE) the returned optimal
#' cutpoint will be the mean of the optimal cutpoint and the next highest
#' observation (for direction = ">") or the next lowest observation
#' (for direction = "<") which avoids biasing the optimal cutpoint.
#' @param ... Further arguments that will be passed to metric_func.
#'
#' @examples
#' oc <- cutpointr(suicide, dsi, suicide, gender, method = maximize_spline_metric,
#' df = 5, metric = accuracy)
#' plot_metric(oc)
#' @export
#' @family method functions
#' @name maximize_spline_metric
maximize_spline_metric <- function(data, x, class, metric_func = youden,
                                   pos_class = NULL, neg_class = NULL, direction,
                                   w = NULL, df = NULL, spar = 1,
                                   nknots = cutpoint_knots, df_offset = NULL,
                                   penalty = 1, control_spar = list(),
                                   tol_metric, use_midpoints, ...) {
    metric_name <- as.character(substitute(metric_func))
    optimize_metric(data = data, x = x, class = class, metric_func = metric_func,
                    pos_class = pos_class, neg_class = neg_class, minmax = "max",
                    direction = direction, metric_name = metric_name,
                    w = w, df = df, spar = spar, nknots = nknots,
                    df_offset = df_offset, penalty = penalty,
                    control_spar = control_spar, spline = TRUE,
                    tol_metric = tol_metric, use_midpoints = use_midpoints, ...)
}

#' @rdname maximize_spline_metric
#' @export
minimize_spline_metric <- function(data, x, class, metric_func = youden,
                                   pos_class = NULL, neg_class = NULL, direction,
                                   w = NULL, df = NULL, spar = 1,
                                   nknots = cutpoint_knots,
                                   df_offset = NULL, penalty = 1,
                                   control_spar = list(), tol_metric,
                                   use_midpoints, ...) {
    metric_name <- as.character(substitute(metric_func))
    optimize_metric(data = data, x = x, class = class, metric_func = metric_func,
                    pos_class = pos_class, neg_class = neg_class, minmax = "min",
                    direction = direction, metric_name = metric_name,
                    w = w, df = df, spar = spar, nknots = nknots,
                    df_offset = df_offset, penalty = penalty,
                    control_spar = control_spar, spline = TRUE,
                    tol_metric = tol_metric, use_midpoints = use_midpoints, ...)
}

#' Calculate number of knots to use in spline smoothing
#'
#' This function calculates the number of knots
#' when using smoothing splines for smoothing a function of metric values per
#' cutpoint value. The function for calculating the number of knots is equal
#' to \code{stats::.nknots_smspl} but uses the number of unique cutpoints
#' in the data as n.
#'
#' @param data A data frame
#' @param x (character) The name of the predictor variable
#' @examples
#' cutpoint_knots(suicide, "dsi")
#' @export
cutpoint_knots <- function(data, x) {
    n_cut <- length(unique(data[[x]]))
    n_knots <- stats::.nknots.smspl(n_cut)
    return(n_knots)
}

#' Optimize a metric function in binary classification after smoothing via
#' generalized additive models
#'
#' Given a function for computing a metric in \code{metric_func}, these functions
#' smooth the function of metric value per cutpoint using generalized additive
#' models (as implemented in \pkg{mgcv}), then
#' maximize or minimize the metric by selecting an optimal cutpoint. For further details
#' on the GAM smoothing see \code{?mgcv::gam}.
#' The \code{metric} function should accept the following inputs:
#' \itemize{
#'  \item \code{tp}: vector of number of true positives
#'  \item \code{fp}: vector of number of false positives
#'  \item \code{tn}: vector of number of true negatives
#'  \item \code{fn}: vector of number of false negatives
#' }
#'
#' The above inputs are arrived at by using all unique values in \code{x}, Inf, and
#' -Inf as possible cutpoints for classifying the variable in class.
#'
#' @return A tibble with the columns \code{optimal_cutpoint}, the corresponding metric
#' value and \code{roc_curve}, a nested tibble that includes all possible cutoffs
#' and the corresponding numbers of true and false positives / negatives and
#' all corresponding metric values.
#'
#' @inheritParams oc_youden_normal
#' @param metric_func (function) A function that computes a
#' metric to be maximized. See description.
#' @param formula A GAM formula. See \code{help("gam", package = "mgcv")} for
#' details.
#' @param optimizer An array specifying the numerical optimization method to
#' use to optimize the smoothing parameter estimation criterion (given by method).
#' See \code{help("gam", package = "mgcv")} for details.
#' @param tol_metric All cutpoints will be returned that lead to a metric
#' value in the interval [m_max - tol_metric, m_max + tol_metric] where
#' m_max is the maximum achievable metric value. This can be used to return
#' multiple decent cutpoints and to avoid floating-point problems.
#' @param use_midpoints (logical) If TRUE (default FALSE) the returned optimal
#' cutpoint will be the mean of the optimal cutpoint and the next highest
#' observation (for direction = ">") or the next lowest observation
#' (for direction = "<") which avoids biasing the optimal cutpoint.
#' @param ... Further arguments that will be passed to metric_func or the
#' GAM smoother.
#'
#' @examples
#' oc <- cutpointr(suicide, dsi, suicide, gender, method = maximize_gam_metric,
#' metric = accuracy)
#' plot_metric(oc)
#' @name maximize_gam_metric
#' @family method functions
#' @export
maximize_gam_metric <- function(data, x, class, metric_func = youden,
                                pos_class = NULL, neg_class = NULL, direction,
                                formula = m ~ s(x.sorted),
                                optimizer = c("outer", "newton"),
                                tol_metric, use_midpoints, ...) {
    if (!requireNamespace("mgcv", quietly = TRUE)) {
        stop("mgcv package has to be installed to use GAM smoothing.")
    }
    formula <- as.character(formula)
    metric_name <- as.character(substitute(metric_func))
    optimize_metric(data = data, x = x, class = class, metric_func = metric_func,
                    pos_class = pos_class, neg_class = neg_class, minmax = "max",
                    direction = direction, metric_name = metric_name,
                    formula = formula, optimizer = optimizer, gam = TRUE,
                    tol_metric = tol_metric, use_midpoints = use_midpoints, ...)
}

#' @examples
#' oc <- cutpointr(suicide, dsi, suicide, gender, method = minimize_gam_metric,
#' metric = abs_d_sens_spec)
#' plot_metric(oc)
#' @rdname maximize_gam_metric
#' @export
minimize_gam_metric <- function(data, x, class, metric_func = youden,
                                pos_class = NULL, neg_class = NULL, direction,
                                formula = m ~ s(x.sorted),
                                optimizer = c("outer", "newton"),
                                tol_metric, use_midpoints, ...) {
    if (!requireNamespace("mgcv", quietly = TRUE)) {
        stop("mgcv package has to be installed to use GAM smoothing.")
    }
    metric_name <- as.character(substitute(metric_func))
    optimize_metric(data = data, x = x, class = class, metric_func = metric_func,
                    pos_class = pos_class, neg_class = neg_class, minmax = "min",
                    direction = direction, metric_name = metric_name,
                    formula = formula, optimizer = optimizer, gam = TRUE,
                    tol_metric = tol_metric, use_midpoints = use_midpoints, ...)
}