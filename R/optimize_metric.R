optimize_metric <- function(data, x, class, metric_func = youden,
                            pos_class = NULL, neg_class = NULL, minmax,
                            direction, metric_name = "metric", loess = FALSE,
                            return_roc = TRUE,
                            ...) {
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
        if (is.null(args$criterion)) args$criterion = "aicc"
        if (is.null(args$degree)) args$degree = 1
        if (is.null(args$family)) args$family = "gaussian"
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
        m <- rep(NA, nrow(roccurve))
        m[finite_roc] <- mod$fitted
    }
    if (minmax == "max") {
        max_m <- max(m, na.rm = TRUE)
        opt <- which(m == max_m)
        oc <- min(roccurve[, "x.sorted"][opt])
        if (length(opt) > 1) {
            message(paste("Multiple optimal cutpoints found, returning minimum of:",
                          paste(roccurve[, "x.sorted"][opt], collapse = ", ")))
        }
        m_oc <- max_m
    } else if (minmax == "min") {
        min_m <- min(m, na.rm = TRUE)
        opt <- which(m == min_m)
        oc <- max(roccurve[, "x.sorted"][opt])
        if (length(opt) > 1) {
            message(paste("Multiple optimal cutpoints found, returning maximum of:",
                          paste(roccurve[, "x.sorted"][opt], collapse = ", ")))
        }
        m_oc <- min_m
    }
    res <- tibble::tibble(optimal_cutpoint = oc)
    if (return_roc) {
        res <- dplyr::bind_cols(res, tidyr::nest_(roccurve, key_col = "roc_curve"))
    }
    if (loess) metric_name <- paste0("loess_", metric_name)
    res[, metric_name] <- m_oc
    return(res)
}

#' Maximize a metric function in binary classification
#'
#' Given a function for computing a metric in metric_func, this function
#' maximizes that metric by selecting an optimal cutpoint.
#' The metric function should accept the following inputs:
#' \itemize{
#'  \item tp: vector of number of true positives
#'  \item fp: vector of number of false positives
#'  \item tn: vector of number of true negatives
#'  \item fn: vector of number of false negatives
#' }
#'
#' The above inputs are arrived at by using all unique values in x, Inf, and
#' -Inf as possible cutpoints for classifying the variable in class.
#'
#' @return A tibble with the columns optimal_cutpoint, the corresponding metric
#' value and roccurve, a nested tibble that includes all possible cutoffs
#' and the corresponding numbers of true and false positives / negatives and
#' all corresponding metric values.
#'
#' @inheritParams oc_youden_normal
#' @param metric_func (function) A function that computes a single number
#' metric to be maximized. See description.
#' @param ... Further arguments that will be passed to metric_func
#' @examples
#' cutpointr(suicide, dsi, suicide, method = maximize_metric, metric = accuracy)
#' @export
maximize_metric <- function(data, x, class, metric_func = youden,
                            pos_class = NULL, neg_class = NULL,
                            direction, ...) {
    metric_name <- as.character(substitute(metric_func))
    optimize_metric(data = data, x = x, class = class, metric_func = metric_func,
                    pos_class = pos_class, neg_class = neg_class, minmax = "max",
                    direction = direction, metric_name = metric_name, ...)
}


#' Minimize a metric function in binary classification
#'
#' Given a function for computing a metric in metric_func, this function
#' minimizes that metric by selecting an optimal cutpoint.
#' The metric function should accept the following inputs:
#' \itemize{
#'  \item tp: vector of number of true positives
#'  \item fp: vector of number of false positives
#'  \item tn: vector of number of true negatives
#'  \item fn: vector of number of false negatives
#' }
#'
#' The above inputs are arrived at by using all unique values in x, Inf, and
#' -Inf as possible cutoffs for classifying the variable in class.
#'
#' @return A tibble with the columns optimal_cutpoint, the corresponding metric
#' value and roccurve, a nested tibble that includes all possible cutoffs
#' and the corresponding numbers of true and false positives / negatives and
#' all corresponding metric values.
#'
#' @inheritParams oc_youden_normal
#' @param metric_func (function) A function that computes a single number
#' metric to be minimized. See description.
#' @param ... Further arguments that will be passed to metric_func
#' @examples
#' cutpointr(suicide, dsi, suicide, method = minimize_metric, metric = abs_d_sens_spec)
#' @export
minimize_metric <- function(data, x, class, metric_func = youden,
                            pos_class = NULL, neg_class = NULL,
                            direction, ...) {
    metric_name <- as.character(substitute(metric_func))
    optimize_metric(data = data, x = x, class = class, metric_func = metric_func,
                    pos_class = pos_class, neg_class = neg_class, minmax = "min",
                    direction = direction, metric_name = metric_name, ...)
}

#' Maximize a metric function in binary classification after LOESS smoothing
#'
#' Given a function for computing a metric in metric_func, this function
#' smoothes the function of metric value against cutpoint using LOESS. Then, it
#' maximizes the metric by selecting an optimal cutpoint. For further details
#' on the LOESS smoothing see ?fANCOVA::loess.as.
#' The metric function should accept the following inputs:
#' \itemize{
#'  \item tp: vector of number of true positives
#'  \item fp: vector of number of false positives
#'  \item tn: vector of number of true negatives
#'  \item fn: vector of number of false negatives
#' }
#'
#' The above inputs are arrived at by using all unique values in x, Inf, and
#' -Inf as possible cutpoints for classifying the variable in class.
#'
#' @return A tibble with the columns optimal_cutpoint, the corresponding metric
#' value and roccurve, a nested tibble that includes all possible cutoffs
#' and the corresponding numbers of true and false positives / negatives and
#' all corresponding metric values.
#'
#' @inheritParams oc_youden_normal
#' @param metric_func (function) A function that computes a single number
#' metric to be maximized. See description.
#' @param ... Further arguments that will be passed to metric_func and
#' additional arguments for the LOESS smoother:
#' \itemize{
#'  \item criterion: the criterion for automatic smoothing parameter selection:
#'  "aicc" denotes bias-corrected AIC criterion, "gcv" denotes generalized
#'  cross-validation.
#'  \item degree: the degree of the local polynomials to be used. It can be
#'  0, 1 or 2.
#'  \item family: if "gaussian" fitting is by least-squares, and if "symmetric"
#'  a re-descending M estimator is used with Tukey's biweight function.
#'  \item user.span: the user-defined parameter which controls the degree of
#'  smoothing.
#' }
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
#' @export
maximize_loess_metric <- function(data, x, class, metric_func = youden,
                            pos_class = NULL, neg_class = NULL, direction,
                            ...) {
    metric_name <- as.character(substitute(metric_func))
    optimize_metric(data = data, x = x, class = class, metric_func = metric_func,
                    pos_class = pos_class, neg_class = neg_class, minmax = "max",
                    direction = direction, metric_name = metric_name,
                    loess = TRUE, ...)
}


#' Minimize a metric function in binary classification after LOESS smoothing
#'
#' Given a function for computing a metric in metric_func, this function
#' smoothes the function of metric value against cutpoint using LOESS. Then, it
#' minimizes the metric by selecting an optimal cutpoint. For further details
#' on the LOESS smoothing see ?fANCOVA::loess.as.
#' The metric function should accept the following inputs:
#' \itemize{
#'  \item tp: vector of number of true positives
#'  \item fp: vector of number of false positives
#'  \item tn: vector of number of true negatives
#'  \item fn: vector of number of false negatives
#' }
#'
#' The above inputs are arrived at by using all unique values in x, Inf, and
#' -Inf as possible cutpoints for classifying the variable in class.
#'
#' @return A tibble with the columns optimal_cutpoint, the corresponding metric
#' value and roccurve, a nested tibble that includes all possible cutoffs
#' and the corresponding numbers of true and false positives / negatives and
#' all corresponding metric values.
#'
#' @inheritParams oc_youden_normal
#' @param metric_func (function) A function that computes a single number
#' metric to be maximized. See description.
#' @param ... Further arguments that will be passed to metric_func and
#' additional arguments for the LOESS smoother:
#' \itemize{
#'  \item criterion: the criterion for automatic smoothing parameter selection:
#'  "aicc" denotes bias-corrected AIC criterion, "gcv" denotes generalized
#'  cross-validation.
#'  \item degree: the degree of the local polynomials to be used. It can be
#'  0, 1 or 2.
#'  \item family: if "gaussian" fitting is by least-squares, and if "symmetric"
#'  a re-descending M estimator is used with Tukey's biweight function.
#'  \item user.span: the user-defined parameter which controls the degree of
#'  smoothing.
#' }
#'
#' @source Xiao-Feng Wang (2010). fANCOVA: Nonparametric Analysis of Covariance.
#'  https://CRAN.R-project.org/package=fANCOVA
#' @source Leeflang, M. M., Moons, K. G., Reitsma, J. B., & Zwinderman, A. H.
#' (2008). Bias in sensitivity and specificity caused by data-driven selection
#' of optimal cutoff values: mechanisms, magnitude, and solutions.
#' Clinical Chemistry, (4), 729–738.
#' @examples
#' oc <- cutpointr(suicide, dsi, suicide, gender, method = minimize_loess_metric,
#' criterion = "aicc", family = "symmetric", degree = 2, user.span = 0.7,
#' metric = misclassification_cost, cost_fp = 1, cost_fn = 10)
#' plot_metric(oc)
#' @export
minimize_loess_metric <- function(data, x, class, metric_func = youden,
                            pos_class = NULL, neg_class = NULL, direction,
                            ...) {
    metric_name <- as.character(substitute(metric_func))
    optimize_metric(data = data, x = x, class = class, metric_func = metric_func,
                    pos_class = pos_class, neg_class = neg_class, minmax = "min",
                    direction = direction, metric_name = metric_name,
                    loess = TRUE, ...)
}


#' Maximize a metric function in binary classification after bootstrapping
#'
#' Given a function for computing a metric in metric_func, this function
#' bootstraps the data boot_cut times and
#' minimizes the metric by selecting an optimal cutpoint. The returned
#' optimal cutpoint is the mean of all optimal cutpoints that were
#' determined in the bootstrap samples.
#'
#' The reported metric represents the usual in-sample performance of the
#' determined cutpoint.
#'
#' The metric function should accept the following inputs:
#' \itemize{
#'  \item tp: vector of number of true positives
#'  \item fp: vector of number of false positives
#'  \item tn: vector of number of true negatives
#'  \item fn: vector of number of false negatives
#' }
#'
#' The above inputs are arrived at by using all unique values in x, Inf, and
#' -Inf as possible cutpoints for classifying the variable in class.
#'
#' @return A tibble with the column optimal_cutpoint
#'
#' @inheritParams oc_youden_normal
#' @param metric_func (function) A function that computes a single number
#' metric to be maximized. See description.
#' @param boot_cut (numeric) Number of bootstrap repetitions over which the mean
#' optimal cutpoint is calculated.
#'
#' @export
maximize_boot_metric <- function(data, x, class, metric_func = youden,
                            pos_class = NULL, neg_class = NULL, direction,
                            boot_cut = 200,
                            ...) {
    metric_name <- as.character(substitute(metric_func))
    optimal_cutpoints <- rep(NA, boot_cut)
    for (i in 1:boot_cut) {
        b_ind <- sample(1:boot_cut, size = boot_cut, replace = TRUE)
        opt_cut <- optimize_metric(data = data[b_ind, ],
                                   x = x, class = class,
                                   metric_func = metric_func,
                                   pos_class = pos_class, neg_class = neg_class,
                                   minmax = "max", direction = direction,
                                   metric_name = metric_name,
                                   return_roc = FALSE, ...)
        optimal_cutpoints[i] <- opt_cut$optimal_cutpoint
    }
    return(tibble::tibble(optimal_cutpoint = mean(optimal_cutpoints)))
}


#' Minimize a metric function in binary classification after bootstrapping
#'
#' Given a function for computing a metric in metric_func, this function
#' bootstraps the data boot_cut times and
#' minimizes the metric by selecting an optimal cutpoint. The returned
#' optimal cutpoint is the mean of all optimal cutpoints that were
#' determined in the bootstrap samples.
#'
#' The metric function should accept the following inputs:
#' \itemize{
#'  \item tp: vector of number of true positives
#'  \item fp: vector of number of false positives
#'  \item tn: vector of number of true negatives
#'  \item fn: vector of number of false negatives
#' }
#'
#' The above inputs are arrived at by using all unique values in x, Inf, and
#' -Inf as possible cutpoints for classifying the variable in class.
#'
#' @return A tibble with the column optimal_cutpoint
#'
#' @inheritParams oc_youden_normal
#' @param metric_func (function) A function that computes a single number
#' metric to be maximized. See description.
#' @param boot_cut (numeric) Number of bootstrap repetitions over which the mean
#' optimal cutpoint is calculated.
#'
#' @export
minimize_boot_metric <- function(data, x, class, metric_func = youden,
                            pos_class = NULL, neg_class = NULL, direction,
                            boot_cut = 200,
                            ...) {
    metric_name <- as.character(substitute(metric_func))
    optimal_cutpoints <- rep(NA, boot_cut)
    for (i in 1:boot_cut) {
        b_ind <- sample(1:boot_cut, size = boot_cut, replace = TRUE)
        opt_cut <- optimize_metric(data = data[b_ind, ],
                                   x = x, class = class,
                                   metric_func = metric_func,
                                   pos_class = pos_class, neg_class = neg_class,
                                   minmax = "min", direction = direction,
                                   metric_name = metric_name,
                                   return_roc = FALSE, ...)
        optimal_cutpoints[i] <- opt_cut$optimal_cutpoint
    }
    return(tibble::tibble(optimal_cutpoint = mean(optimal_cutpoints)))
}


