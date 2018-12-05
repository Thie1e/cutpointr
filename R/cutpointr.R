#' Determine and evaluate optimal cutpoints
#'
#' Using predictions (or e.g. biological marker values) and binary class labels, this function
#' will determine "optimal" cutpoints using various selectable methods. The
#' methods for cutpoint determination can be evaluated using bootstrapping. An
#' estimate of the cutpoint variability and the out-of-sample performance will then
#' be returned.
#'
#' If \code{direction} and/or \code{pos_class} and \code{neg_class} are not given, the function will
#' assume that higher values indicate the positive class and use the class
#' with a higher median as the positive class.
#'
#' Different methods can be selected for determining the optimal cutpoint via
#' the method argument. The package includes the following method functions:
#' \itemize{
#'  \item \code{maximize_metric}: Maximize the metric function
#'  \item \code{minimize_metric}: Minimize the metric function
#'  \item \code{maximize_loess_metric}: Maximize the metric function after LOESS
#'  smoothing
#'  \item \code{minimize_loess_metric}: Minimize the metric function after LOESS
#'  smoothing
#'  \item \code{maximize_spline_metric}: Maximize the metric function after spline
#'  smoothing
#'  \item \code{minimize_spline_metric}: Minimize the metric function after spline
#'  smoothing
#'  \item \code{maximize_boot_metric}: Maximize the metric function as a summary of
#'  the optimal cutpoints in bootstrapped samples
#'  \item \code{minimize_boot_metric}: Minimize the metric function as a summary of
#'  the optimal cutpoints in bootstrapped samples
#'  \item \code{oc_youden_kernel}: Maximize the Youden-Index after kernel smoothing
#'  the distributions of the two classes
#'  \item \code{oc_youden_normal}: Maximize the Youden-Index parametrically
#'  assuming normally distributed data in both classes
#'  \item \code{oc_manual}: Specify the cutpoint manually
#' }
#'
#' User-defined functions can be supplied to method, too. As a reference,
#' the code of all included method functions can be accessed by simply typing
#' their name. To define a new method function, create a function that may take
#' as input(s):
#' \itemize{
#'  \item \code{data}: A \code{data.frame} or \code{tbl_df}
#'  \item \code{x}: (character) The name of the predictor or independent variable
#'  \item \code{class}: (character) The name of the class or dependent variable
#'  \item \code{metric_func}: A function for calculating a metric, e.g. accuracy
#'  \item \code{pos_class}: The positive class
#'  \item \code{neg_class}: The negative class
#'  \item \code{direction}: ">=" if the positive class has higher x values, "<=" otherwise
#'  \item \code{tol_metric}: (numeric) In the built-in methods a tolerance around
#'  the optimal metric value
#'  \item \code{use_midpoints}: (logical) In the built-in methods whether to
#'  use midpoints instead of exact optimal cutpoints
#'  \item \code{...} Further arguments
#' }
#'
#' The \code{...} argument can be used to avoid an error if not all of the above
#' arguments are needed or in order to pass additional arguments to method.
#' The function should return a \code{data.frame} or \code{tbl_df} with
#' one row, the column "optimal_cutpoint", and an optional column with an arbitrary name
#' with the metric value at the optimal cutpoint.
#'
#' Built-in metric functions include:
#' \itemize{
#'  \item \code{accuracy}: Fraction correctly classified
#'  \item \code{youden}: Youden- or J-Index = sensitivity + specificity - 1
#'  \item \code{sum_sens_spec}: sensitivity + specificity
#'  \item \code{sum_ppv_npv}: The sum of positive predictive value (PPV) and negative
#'  predictive value (NPV)
#'  \item \code{prod_sens_spec}: sensitivity * specificity
#'  \item \code{prod_ppv_npv}: The product of positive predictive value (PPV) and
#'  negative predictive value (NPV)
#'  \item \code{cohens_kappa}: Cohen's Kappa
#'  \item \code{abs_d_sens_spec}: The absolute difference between
#'  sensitivity and specificity
#'  \item \code{roc01}: Distance to the point (0,1) on ROC space
#'  \item \code{abs_d_ppv_npv}: The absolute difference between positive predictive
#'  value (PPV) and negative predictive value (NPV)
#'  \item \code{p_chisquared}: The p-value of a chi-squared test on the confusion
#'  matrix of predictions and observations
#'  \item \code{odds_ratio}: The odds ratio calculated as (TP / FP) / (FN / TN)
#'  \item \code{risk_ratio}: The risk ratio (relative risk) calculated as
#'  (TP / (TP + FN)) / (FP / (FP + TN))
#'  \item positive and negative likelihood ratio calculated as
#'  \code{plr} = true positive rate / false positive rate and
#'  \code{nlr} = false negative rate / true negative rate
#'  \item \code{misclassification_cost}: The sum of the misclassification cost of
#'  false positives and false negatives fp * cost_fp + fn * cost_fn.
#'  Additional arguments to cutpointr: \code{cost_fp}, \code{cost_fn}
#'  \item \code{total_utility}: The total utility of true / false positives / negatives
#'  calculated as utility_tp * TP + utility_tn * TN - cost_fp * FP - cost_fn * FN.
#'  Additional arguments to cutpointr: \code{utility_tp}, \code{utility_tn},
#'  \code{cost_fp}, \code{cost_fn}
#'  \item \code{F1_score}: The F1-score (2 * TP) / (2 * TP + FP + FN)
#' }
#'
#' Furthermore, the following functions are included which can be used as metric
#' functions but are more useful for plotting purposes, for example in
#' plot_cutpointr, or for defining new metric functions:
#' \code{tp}, \code{fp}, \code{tn}, \code{fn}, \code{tpr}, \code{fpr},
#' \code{tnr}, \code{fnr}, \code{false_omission_rate},
#' \code{false_discovery_rate}, \code{ppv}, \code{npv}, \code{precision},
#' \code{recall}, \code{sensitivity}, and \code{specificity}.
#'
#' User defined metric functions can be created as well which can accept the following
#' inputs as vectors:
#' \itemize{
#'  \item \code{tp}: Vector of true positives
#'  \item \code{fp}: Vector of false positives
#'  \item \code{tn}: Vector of true negatives
#'  \item \code{fn}: Vector of false negatives
#'  \item \code{...} If the metric function is used in conjunction with any of the
#'  maximize / minimize methods, further arguments can be passed
#' }
#'
#' The function should return a numeric vector or a matrix or a \code{data.frame}
#' with one column. If the column is named,
#' the name will be included in the output and plots. Avoid using names that
#' are identical to the column names that are by default returned by \pkg{cutpointr}.
#'
#' If \code{boot_runs} is positive, that number of bootstrap samples will be drawn
#' and the optimal cutpoint using \code{method} will be determined. Additionally,
#' as a way of internal validation, the function in \code{metric} will be used to
#' score the out-of-bag predictions using the cutpoints determined by
#' \code{method}. Various default metrics are always included in the bootstrap results.
#'
#' If multiple optimal cutpoints are found, the column optimal_cutpoint becomes a
#' list that contains the vector(s) of the optimal cutpoints.
#'
#' If \code{use_midpoints = TRUE} the mean of the optimal cutpoint and the next
#' highest or lowest possible cutpoint is returned, depending on \code{direction}.
#'
#' The \code{tol_metric} argument can be used to avoid floating-point problems
#' that may lead to exclusion of cutpoints that achieve the optimally achievable
#' metric value. Additionally, by selecting a large tolerance multiple cutpoints
#' can be returned that lead to decent metric values in the vicinity of the
#' optimal metric value. \code{tol_metric} is passed to metric and is only
#' supported by the maximization and minimization functions, i.e.
#' \code{maximize_metric}, \code{minimize_metric}, \code{maximize_loess_metric},
#' \code{minimize_loess_metric}, \code{maximize_spline_metric}, and
#' \code{minimize_spline_metric}. In \code{maximize_boot_metric} and
#' \code{minimize_boot_metric} multiple optimal cutpoints will be passed to the
#' \code{summary_func} of these two functions.
#'
#' @examples
#' library(cutpointr)
#'
#' ## Optimal cutpoint for dsi
#' data(suicide)
#' opt_cut <- cutpointr(suicide, dsi, suicide)
#' opt_cut
#' summary(opt_cut)
#' plot(opt_cut)
#'
#' \dontrun{
#' ## Predict class for new observations
#' predict(opt_cut, newdata = data.frame(dsi = 0:5))
#'
#' ## Supplying raw data, same result
#' cutpointr(x = suicide$dsi, class = suicide$suicide)
#'
#' ## direction, class labels, method and metric can be defined manually
#' ## Again, same result
#' cutpointr(suicide, dsi, suicide, direction = ">=", pos_class = "yes",
#'           method = maximize_metric, metric = youden)
#'
#' ## Optimal cutpoint for dsi, as before, but for the separate subgroups
#' opt_cut <- cutpointr(suicide, dsi, suicide, gender)
#' opt_cut
#'
#' ## Bootstrapping also works on individual subgroups
#' ## low boot_runs for illustrative purposes
#' set.seed(30)
#' opt_cut <- cutpointr(suicide, dsi, suicide, gender, boot_runs = 5)
#' opt_cut
#' summary(opt_cut)
#' plot(opt_cut)
#'
#' ## Transforming variables (unrealistic, just to show the functionality)
#' opt_cut <- cutpointr(suicide, x = log(dsi + 1), class = suicide == "yes",
#'     subgroup = dsi %% 2 == 0)
#' opt_cut
#' predict(opt_cut, newdata = data.frame(dsi = 1:3))
#'
#' ## Parallelized bootstrapping
#'   cl <- makeCluster(2) # 2 cores
#'   registerDoParallel(cl)
#'   registerDoRNG(12) # Reproducible parallel loops using doRNG
#'   opt_cut <- cutpointr(suicide, dsi, suicide, gender,
#'                        boot_runs = 10, allowParallel = TRUE)
#'   stopCluster(cl)
#'   opt_cut
#'   plot(opt_cut)
#'
#' ## Robust cutpoint method using kernel smoothing for optimizing Youden-Index
#' opt_cut <- cutpointr(suicide, dsi, suicide, gender,
#'                      method = oc_youden_kernel)
#' opt_cut
#' }
#'
#'
#'
#' @param data A data.frame with the data needed for x, class and subgroup.
#' @param x The variable name without quotes to be used for classification,
#'  e.g. predictions, or an expression. The raw vector of values if the data argument
#'  is unused.
#' @param class The variable name without quotes indicating class membership
#' or an expression. The raw vector of values if the data argument is unused.
#' @param subgroup An additional covariate that identifies subgroups or the raw data if
#' data = NULL. Separate optimal cutpoints will be determined per group.
#' Numeric, character and factor are allowed.
#' @param method (function) A function for determining cutpoints. Can
#' be user supplied or use some of the built in methods. See details.
#' @param metric (function) The function for computing a metric when using
#' maximize_metric or minimize_metric as method and and for the
#' out-of-bag values during bootstrapping. A way of internally validating the performance.
#' User defined functions can be supplied, see details.
#' @param pos_class (optional) The value of class that indicates the positive class.
#' @param neg_class (optional) The value of class that indicates the negative class.
#' @param direction (character, optional) Use ">=" or "<=" to indicate whether x
#' is supposed to be larger or smaller for the positive class.
#' @param boot_runs (numerical) If positive, this number of bootstrap samples
#' will be used to assess the variability and the out-of-sample performance.
#' @param use_midpoints (logical) If TRUE (default FALSE) the returned optimal
#' cutpoint will be the mean of the optimal cutpoint and the next highest
#' observation (for direction = ">=") or the next lowest observation
#' (for direction = "<=") which avoids biasing the optimal cutpoint.
#' @param break_ties If multiple cutpoints are found, they can be summarized using
#' this function, e.g. mean or median. To return all cutpoints use c as the function.
#' @param na.rm (logical) Set to TRUE (default FALSE) to keep only complete
#' cases of x, class and subgroup (if specified). Missing values with
#' na.rm = FALSE will raise an error.
#' @param allowParallel (logical) If TRUE, the bootstrapping will be parallelized
#' using foreach. A local cluster, for example, should be started manually
#' beforehand.
#' @param silent (logical) If TRUE suppresses all messages.
#' @param tol_metric All cutpoints will be returned that lead to a metric
#' value in the interval [m_max - tol_metric, m_max + tol_metric] where
#' m_max is the maximum achievable metric value. This can be used to return
#' multiple decent cutpoints and to avoid floating-point problems. Not supported
#' by all \code{method} functions, see details.
#' @param ... Further optional arguments that will be passed to method.
#' minimize_metric and maximize_metric pass ... to metric.
#' @importFrom purrr %>%
#' @importFrom foreach %do%
#' @return A cutpointr object which is also a data.frame and tbl_df.
#' @useDynLib cutpointr
#' @importFrom Rcpp sourceCpp
#' @family main cutpointr functions
#' @export cutpointr
cutpointr <- function(...) {
    UseMethod("cutpointr")
}

#' @rdname cutpointr
#' @importFrom stats median
#' @export
cutpointr.default <- function(data, x, class, subgroup = NULL,
                      method = maximize_metric, metric = sum_sens_spec,
                      pos_class = NULL, neg_class = NULL, direction = NULL,
                      boot_runs = 0, use_midpoints = FALSE,
                      break_ties = c, na.rm = FALSE,
                      allowParallel = FALSE, silent = FALSE,
                      tol_metric = 1e-6, ...) {

    #
    # NSE
    #
    predictor <- deparse(substitute(x))
    outcome   <- deparse(substitute(class))
    x <- tryCatch(eval(substitute(x), data, parent.frame()),
                  error = function(cond) {
                      stop(paste("If the data argument is specified,",
                                 "x and class should refer to columns",
                                 "in data."))
                  })
    class <- tryCatch(eval(substitute(class), data, parent.frame()),
                      error = function(cond) {
                          stop(paste("If the data argument is specified,",
                                     "x and class should refer to columns",
                                     "in data."))
                      })
    subgroup_var <- deparse(substitute(subgroup))
    subgroup <- tryCatch(eval(substitute(subgroup), data, parent.frame()),
                         error = function(cond) {
                             stop(paste("If the data argument is specified,",
                                        "subgroup should refer to a column",
                                        "in data."))
                         })

    # Get method function
    if (length(method) > 1 | !(class(method) == "function")) {
        stop("method should be a function")
    } else {
        cl <- match.call()
        mod_name <- cl$method
        # if default was not changed:
        mod_name <- as.character(substitute(method))
    }
    if (is.null(mod_name)) stop("Could not get the method function")

    # Get metric function
    if (length(metric) > 1 | !(class(metric) == "function")) {
        stop("metric should be a function")
    }
    cl <- match.call()
    metric_name <- cl$metric
    # if default was not changed:
    metric_name <- as.character(substitute(metric))
    if (is.null(metric_name)) stop("Could not get the metric function")
    if (silent) {
        suppressMessages(
            cutpointr_internal(x, class, subgroup, method, metric, pos_class, neg_class,
                               direction, boot_runs, use_midpoints, break_ties, na.rm,
                               allowParallel, predictor, outcome, mod_name, subgroup_var,
                               tol_metric, ...)
        )
    } else {
        cutpointr_internal(x, class, subgroup, method, metric, pos_class, neg_class,
                           direction, boot_runs, use_midpoints, break_ties, na.rm,
                           allowParallel, predictor, outcome, mod_name, subgroup_var,
                           tol_metric, ...)
    }
}

#' @rdname cutpointr
#' @importFrom stats median
#' @export
cutpointr.numeric <- function(x, class, subgroup = NULL,
                      method = maximize_metric, metric = sum_sens_spec,
                      pos_class = NULL, neg_class = NULL, direction = NULL,
                      boot_runs = 0, use_midpoints = FALSE,
                      break_ties = median, na.rm = FALSE,
                      allowParallel = FALSE, silent = FALSE,
                      tol_metric = 1e-6, ...) {
    predictor <- "x"
    outcome <- "class"
    subgroup_var <- "subgroup"

    # Get method function
    if (length(method) > 1 | !(class(method) == "function")) {
        stop("method should be a function")
    } else {
        cl <- match.call()
        mod_name <- cl$method
        # if default was not changed:
        mod_name <- as.character(substitute(method))
    }
    if (is.null(mod_name)) stop("Could not get the method function")

    # Get metric function
    if (length(metric) > 1 | !(class(metric) == "function")) {
        stop("metric should be a function")
    }
    cl <- match.call()
    metric_name <- cl$metric
    # if default was not changed:
    metric_name <- as.character(substitute(metric))
    if (is.null(metric_name)) stop("Could not get the metric function")
    if (silent) {
        suppressMessages(
            cutpointr_internal(x, class, subgroup, method, metric, pos_class, neg_class,
                               direction, boot_runs, use_midpoints, break_ties, na.rm,
                               allowParallel, predictor, outcome, mod_name, subgroup_var,
                               tol_metric, ...)
        )
    } else {
        cutpointr_internal(x, class, subgroup, method, metric, pos_class, neg_class,
                           direction, boot_runs, use_midpoints, break_ties, na.rm,
                           allowParallel, predictor, outcome, mod_name, subgroup_var,
                           tol_metric, ...)
    }
}



#' The standard evaluation version of cutpointr
#'
#' This function is equivalent to \code{cutpointr} but takes only quoted arguments
#' for \code{x}, \code{class} and \code{subgroup}. This function is suitable for
#' programming with. For details on \code{cutpointr} see help("cutpointr").
#' @inheritParams cutpointr
#' @param x (character) The variable name to be used for
#'  classification, e.g. predictions or test values.
#' @param class (character) The variable name indicating class membership.
#' @param subgroup (character) The variable name
#' of an additional covariate that identifies subgroups. Separate
#' optimal cutpoints will be determined per group.
#' @examples
#' library(cutpointr)
#'
#' ## Optimal cutpoint for dsi
#' data(suicide)
#' opt_cut <- cutpointr_(suicide, "dsi", "suicide")
#' opt_cut
#' summary(opt_cut)
#' plot(opt_cut)
#' predict(opt_cut, newdata = data.frame(dsi = 0:5))
#' @importFrom stats median
#' @family main cutpointr functions
#' @export
cutpointr_ <- function(data, x, class, subgroup = NULL,
                      method = maximize_metric, metric = sum_sens_spec,
                      pos_class = NULL, neg_class = NULL, direction = NULL,
                      boot_runs = 0, use_midpoints = FALSE,
                      break_ties = median, na.rm = FALSE,
                      allowParallel = FALSE, silent = FALSE,
                      tol_metric = 1e-6, ...) {
    #
    # SE
    #
    x <- as.name(x)
    class <- as.name(class)
    if (!is.null(subgroup)) subgroup <- as.name(subgroup)
    predictor <- deparse(substitute(x))
    outcome   <- deparse(substitute(class))
    x <- eval(substitute(x), data, parent.frame())
    class <- eval(substitute(class), data, parent.frame())
    subgroup_var <- deparse(substitute(subgroup))
    subgroup <- eval(substitute(subgroup), data, parent.frame())

    # Get method function
    if (length(method) > 1 | !(class(method) == "function")) {
        stop("method should be a function")
    } else {
        cl <- match.call()
        mod_name <- cl$method
        # if default was not changed:
        mod_name <- as.character(substitute(method))
    }
    if (is.null(mod_name)) stop("Could not get the method function")

    # Get metric function
    if (length(metric) > 1 | !(class(metric) == "function")) {
        stop("metric should be a function")
    }
    cl <- match.call()
    metric_name <- cl$metric
    # if default was not changed:
    metric_name <- as.character(substitute(metric))
    if (is.null(metric_name)) stop("Could not get the metric function")

    if (silent) {
        suppressMessages(
            cutpointr_internal(x, class, subgroup, method, metric, pos_class, neg_class,
                               direction, boot_runs, use_midpoints, break_ties, na.rm,
                               allowParallel, predictor, outcome, mod_name, subgroup_var,
                               tol_metric = 1e-6, ...)
        )
    } else {
        cutpointr_internal(x, class, subgroup, method, metric, pos_class, neg_class,
                           direction, boot_runs, use_midpoints, break_ties, na.rm,
                           allowParallel, predictor, outcome, mod_name, subgroup_var,
                           tol_metric = 1e-6, ...)
    }
}


cutpointr_internal <- function(x, class, subgroup, method, metric, pos_class,
                               neg_class, direction, boot_runs,
                               use_midpoints, break_ties, na.rm, allowParallel, predictor,
                               outcome, mod_name, subgroup_var,
                               tol_metric, ...) {
    #
    # Prep
    #

    #NA
    if (any(anyNA(c(x, class)) | (!is.null(subgroup) & anyNA(subgroup))) &
         (!na.rm)) {
        stop("NAs found but na.rm = FALSE")
    }

    # Check classes
    if (!is.null(dim(class))) stop("class variable should be a vector")
    if (na.rm) uc <- unique(stats::na.omit(class)) else uc <- unique(class)
    luc <- length(uc)
    if (luc != 2) stop(paste("Expecting two classes, got", luc))
    if (!(is.null(pos_class))) {
        if (!(pos_class %in% class)) stop("pos_class not found in data")
    }
    if (!(is.null(neg_class))) {
        if (!(neg_class %in% class)) stop("neg_class not found in data")
    }

    # Determine direction and/or pos_class if necessary:
    if (any(c(is.null(pos_class), is.null(neg_class), is.null(direction)))) {
        assumptions <- assume_direction_pos_class(x = x, class = class,
                                                  pos_class = pos_class,
                                                  neg_class = neg_class,
                                                  direction = direction,
                                                  na.rm = na.rm,
                                                  uc = uc)
    }
    if (is.null(direction)) direction <- assumptions$direction
    stopifnot(direction %in% c("<", ">", ">=", "<="))
    if (is.null(pos_class)) pos_class <- assumptions$pos_class
    if (is.null(neg_class)) neg_class <- assumptions$neg_class

    #
    # Calculate optimal cutpoint, map to cutpoint function
    #
    if (!is.null(subgroup)) {
        dat <- data.frame(x = x, class = class, subgroup = subgroup,
                          stringsAsFactors = FALSE)
        colnames(dat) <- c(predictor, outcome, "subgroup")
        if (na.rm) dat <- stats::na.omit(dat)
        g <- unique(dat$subgroup)
        dat <- dat %>%
            dplyr::group_by_("subgroup") %>%
            tidyr::nest_(key_col = "data") %>%
            dplyr::mutate_(subgroup = ~ as.character(subgroup))
        dat$pos_class <- pos_class
        optcut <- purrr::pmap(list(dat$subgroup, dat$data), function(g, d) {
            if (nrow(d) <= 1) stop(paste("Subgroup", g, "has <= 1 observations"))
            # optcut <- data.frame(subgroup = g, stringsAsFactors = FALSE)
            optcut <- tibble::tibble(subgroup = g)
            method_result <- method(data = d, x = predictor, class = outcome,
                                    metric_func = metric,
                                    direction = direction, pos_class = pos_class,
                                    neg_class = neg_class, tol_metric = tol_metric,
                                    use_midpoints = use_midpoints,
                                    ...)
            method_result <- check_method_cols(method_result)
            optcut <- dplyr::bind_cols(optcut, method_result)
            if (length(optcut[["optimal_cutpoint"]][[1]]) > 1) {
                message("Multiple optimal cutpoints found")
            }
            optcut <- apply_break_ties(optcut, break_ties)
            # Depending on method the roc_curve may be missing
            if (!(has_column(optcut, "roc_curve"))) {
                roc_curve <- roc(data = d, x = predictor, class = outcome,
                                 pos_class = pos_class, neg_class = neg_class,
                                 direction = direction)
                roc_curve <- tidyr::nest_(roc_curve, key_col = "roc_curve")
                optcut <- dplyr::bind_cols(roc_curve,
                                           tibble::as_tibble(optcut))
            } else {
                check_roc_curve(optcut)
            }
            # If no metric is returned
            if (ncol(optcut) <= 3) {
                opt_ind <- get_opt_ind(optcut$roc_curve[[1]],
                                       oc = unlist(optcut$optimal_cutpoint),
                                       direction = direction)
                m <- metric(tp = optcut$roc_curve[[1]]$tp[opt_ind],
                            fp = optcut$roc_curve[[1]]$fp[opt_ind],
                            tn = optcut$roc_curve[[1]]$tn[opt_ind],
                            fn = optcut$roc_curve[[1]]$fn[opt_ind])
                m <- check_metric_name(m)
                colnames(m) <- make.names(colnames(m))
                optcut <- dplyr::bind_cols(optcut, tibble::as_tibble(m))
            }
            optcut <- check_colnames(optcut)
            # Breaking ties may have altered the cutpoints. Recalculate main metric
            opt_ind <- get_opt_ind(optcut$roc_curve[[1]],
                                   oc = unlist(optcut$optimal_cutpoint),
                                   direction = direction)
            m <- metric(tp = optcut$roc_curve[[1]]$tp[opt_ind],
                        fp = optcut$roc_curve[[1]]$fp[opt_ind],
                        tn = optcut$roc_curve[[1]]$tn[opt_ind],
                        fn = optcut$roc_curve[[1]]$fn[opt_ind])
            optcut <- add_list(optcut, as.numeric(m), optcut$metric_name)
            sesp <- sesp_from_oc(optcut$roc_curve[[1]],
                                 oc = optcut$optimal_cutpoint,
                                 direction = direction)
            optcut <- add_list(optcut, sesp[, "sensitivity"], "sensitivity")
            optcut <- add_list(optcut, sesp[, "specificity"], "specificity")
            acc <- accuracy_from_oc(optcut$roc_curve[[1]],
                                    oc = optcut$optimal_cutpoint[[1]],
                                    direction = direction)[, "accuracy"]
            optcut <- add_list(optcut, acc, "acc")
            return(optcut)
        })
        # if multiple cutpoints only in some groups, all cols have to be lists
        coltypes <- purrr::map_chr(optcut, function(x) class(x[2][[1]]))
        if (any(coltypes == "list") & any(coltypes == "numeric")) {
            optcut <- purrr::map(optcut, function(x) {
                if (is.numeric(x$optimal_cutpoint)) {
                    x$optimal_cutpoint <- list(x$optimal_cutpoint)
                    x[[x$metric_name]] <- list(x[[x$metric_name]])
                    x$sensitivity <- list(x$sensitivity)
                    x$specificity <- list(x$specificity)
                    x$acc <- list(x$acc)
                }
                return(x)
            })
        }
        optcut <- dplyr::bind_rows(optcut)
        optcut <- optcut %>%
            dplyr::mutate_(
                AUC = ~ purrr::map_dbl(roc_curve, function(r) {
                    auc(tpr = r$tpr, fpr = r$fpr)
                }),
                prevalence = ~ purrr::map_dbl(roc_curve, function(r) {
                    utils::tail(r$tp, 1) / (utils::tail(r$tp, 1) + utils::tail(r$fp, 1))
                })
            )
        optcut <- tibble::as_tibble(optcut)
        optcut <- dplyr::full_join(optcut, dat, by = "subgroup")
    } else {
        dat <- tibble::tibble_(list(x = ~ x, class = ~ class))
        colnames(dat) <- c(predictor, outcome)
        if (na.rm) dat <- stats::na.omit(dat)
        dat <- dat %>%
            tidyr::nest_(key_col = "data")
        dat$pos_class <- pos_class
        optcut <- method(data = dat$data[[1]],  x = predictor, class = outcome,
                         metric_func = metric,
                         direction = direction, pos_class = pos_class,
                         neg_class = neg_class, tol_metric = tol_metric,
                         use_midpoints = use_midpoints, ...)
        optcut <- check_method_cols(optcut)
        if (length(optcut[["optimal_cutpoint"]][[1]]) > 1) {
            message("Multiple optimal cutpoints found")
        }
        optcut <- apply_break_ties(optcut, break_ties)
        if (!(has_column(optcut, "roc_curve"))) {
            roc_curve <- roc(data = dat$data[[1]],
                             x = predictor, class = outcome,
                             pos_class = pos_class, neg_class = neg_class,
                             direction = direction)
            roc_curve <- tidyr::nest_(roc_curve, key_col = "roc_curve",
                                      nest_cols = colnames(roc_curve))
            optcut <- dplyr::bind_cols(roc_curve,
                                       tibble::as_tibble(optcut))
        } else {
            check_roc_curve(optcut)
        }
        # If no metric is returned
        if (ncol(optcut) <= 2) {
            opt_ind <- get_opt_ind(optcut$roc_curve[[1]],
                                   oc = unlist(optcut$optimal_cutpoint),
                                   direction = direction)
            m <- metric(tp = optcut$roc_curve[[1]]$tp[opt_ind],
                        fp = optcut$roc_curve[[1]]$fp[opt_ind],
                        tn = optcut$roc_curve[[1]]$tn[opt_ind],
                        fn = optcut$roc_curve[[1]]$fn[opt_ind])
            m <- check_metric_name(m)
            colnames(m) <- make.names(colnames(m))
            optcut <- dplyr::bind_cols(optcut, tibble::as_tibble(m))
        }
        optcut <- check_colnames(optcut)
        # Breaking ties may have altered the cutpoints. Recalculate main metric
        opt_ind <- get_opt_ind(optcut$roc_curve[[1]],
                               oc = unlist(optcut$optimal_cutpoint),
                               direction = direction)
        m <- metric(tp = optcut$roc_curve[[1]]$tp[opt_ind],
                    fp = optcut$roc_curve[[1]]$fp[opt_ind],
                    tn = optcut$roc_curve[[1]]$tn[opt_ind],
                    fn = optcut$roc_curve[[1]]$fn[opt_ind])
        optcut <- add_list(optcut, as.numeric(m), optcut$metric_name)
        sesp <- sesp_from_oc(optcut$roc_curve[[1]],
                             oc = optcut$optimal_cutpoint,
                             direction = direction)
        optcut <- add_list(optcut, sesp[, "sensitivity"], "sensitivity")
        optcut <- add_list(optcut, sesp[, "specificity"], "specificity")
        acc <- accuracy_from_oc(optcut$roc_curve[[1]],
                                oc = unlist(optcut$optimal_cutpoint),
                                direction = direction)[, "accuracy"]
        optcut <- add_list(optcut, acc, "acc")
        optcut$AUC <- auc(tpr = optcut$roc_curve[[1]]$tpr,
                          fpr = optcut$roc_curve[[1]]$fpr)
        optcut$prevalence <- utils::tail(optcut$roc_curve[[1]]$tp, 1) /
            (utils::tail(optcut$roc_curve[[1]]$tp, 1) +
                 utils::tail(optcut$roc_curve[[1]]$fp, 1))
        optcut <- tibble::as_tibble(optcut)
        optcut <- dplyr::bind_cols(optcut, dat)
    }


    optcut$direction                        <- direction
    optcut$predictor                        <- predictor
    optcut$outcome                          <- outcome
    optcut$neg_class                        <- neg_class
    optcut$method                           <- mod_name
    if (!is.null(subgroup)) optcut$grouping <- subgroup_var

    # Reorder for nicer output
    mn <- optcut$metric_name[1]
    select_cols <- c("subgroup", "direction", "optimal_cutpoint",
                     "method", mn,
                     "acc", "sensitivity", "specificity", "AUC",
                     "pos_class", "neg_class", "prevalence",
                     "outcome", "predictor", "grouping", "data", "roc_curve")
    # subgroup and grouping may not be given
    select_cols <- select_cols[select_cols %in% colnames(optcut)]
    optcut <- optcut[, select_cols]

    #
    # Bootstrap cutpoint variability and get LOO-Bootstrap performance estimate
    # Data are already nested and grouped if necessary
    #
    if (allowParallel) {
        requireNamespace("foreach")
        `%seq_or_par%` <- doRNG::`%dorng%`
    } else {
        `%seq_or_par%` <- foreach::`%do%`
    }
    if (boot_runs <= 0) {
        bootstrap <- rep(NA, times = nrow(optcut))
    } else {
        message("Running bootstrap...")
        boot_runs <- ceiling(boot_runs)
        bootstrap <- dat %>%
            dplyr::transmute_(boot = ~ purrr::map2(dat$data, dat$pos_class,
                                                    function(g, pc) {
                boot_g <- foreach::foreach(rep = 1:boot_runs, .combine = rbind,
                    .export = c("method", "direction", "metric", "break_ties",
                    "neg_class", "mn", "use_midpoints",
                    "predictor", "outcome", "tol_metric")) %seq_or_par%
                    {
                        b_ind <- simple_boot(g, outcome)
                        optcut_b <- method(data = g[b_ind, ], x = predictor,
                                           metric_func = metric,
                                           class = outcome,
                                           direction = direction,
                                           pos_class = pc,
                                           neg_class = neg_class,
                                           tol_metric = tol_metric,
                                           use_midpoints = use_midpoints, ...)
                        optcut_b <- check_method_cols(optcut_b)
                        optcut_b <- tibble::as_tibble(optcut_b)
                        optcut_b <- apply_break_ties(optcut_b, break_ties)
                        # LOO-Bootstrap
                        if (!(has_column(optcut_b, "roc_curve"))) {
                            roc_curve_b <- roc(data = g[b_ind, ], x = predictor,
                                               class = outcome,
                                               pos_class = pc, neg_class = neg_class,
                                               direction = direction, silent = TRUE)
                            roc_curve_b <- tidyr::nest_(roc_curve_b,
                                                        key_col = "roc_curve")
                            optcut_b <- dplyr::bind_cols(optcut_b, roc_curve_b)
                        }
                        opt_ind_b <- get_opt_ind(roc_curve = optcut_b$roc_curve[[1]],
                                                 oc = unlist(optcut_b$optimal_cutpoint),
                                                 direction = direction)
                        auc_b <- auc(tpr = optcut_b$roc_curve[[1]]$tpr,
                                     fpr = optcut_b$roc_curve[[1]]$fpr)
                        Sens_Spec_b <- sesp_from_oc(optcut_b$roc_curve[[1]],
                                                     oc = unlist(optcut_b$optimal_cutpoint),
                                                     direction = direction,
                                                     opt_ind = opt_ind_b)
                        Acc_b <- accuracy_from_oc(optcut_b$roc_curve[[1]],
                                                  oc = unlist(optcut_b$optimal_cutpoint),
                                                  direction = direction,
                                                  opt_ind = opt_ind_b)[, "accuracy"]
                        kap_b <- kappa_from_oc(optcut_b$roc_curve[[1]],
                                               oc = unlist(optcut_b$optimal_cutpoint),
                                               direction = direction,
                                               opt_ind = opt_ind_b)
                        metric_b <- metric(tp = optcut_b$roc_curve[[1]]$tp[opt_ind_b],
                                             fp = optcut_b$roc_curve[[1]]$fp[opt_ind_b],
                                             tn = optcut_b$roc_curve[[1]]$tn[opt_ind_b],
                                             fn = optcut_b$roc_curve[[1]]$fn[opt_ind_b])
                        metric_b <- check_metric_name(metric_b)
                        roc_curve_oob <- roc(data = g[-b_ind, ], x = predictor,
                                             class = outcome,
                                             pos_class = pc, neg_class = neg_class,
                                             direction = direction, silent = TRUE)
                        opt_ind_oob <- get_opt_ind(roc_curve = roc_curve_oob,
                                                 oc = unlist(optcut_b$optimal_cutpoint),
                                                 direction = direction)
                        auc_oob <- auc(tpr = roc_curve_oob$tpr,
                                       fpr = roc_curve_oob$fpr)
                        Sens_Spec_oob <- sesp_from_oc(roc_curve_oob,
                                                     oc = unlist(optcut_b$optimal_cutpoint),
                                                     direction = direction,
                                                     opt_ind = opt_ind_oob)
                        Acc_oob <- accuracy_from_oc(roc_curve_oob,
                                                  oc = unlist(optcut_b$optimal_cutpoint),
                                                  direction = direction,
                                                  opt_ind = opt_ind_oob)[, "accuracy"]
                        kap_oob <- kappa_from_oc(roc_curve_oob,
                                               oc = unlist(optcut_b$optimal_cutpoint),
                                               direction = direction,
                                               opt_ind = opt_ind_oob)
                        metric_oob <- metric(tp = roc_curve_oob$tp[opt_ind_oob],
                                             fp = roc_curve_oob$fp[opt_ind_oob],
                                             tn = roc_curve_oob$tn[opt_ind_oob],
                                             fn = roc_curve_oob$fn[opt_ind_oob])
                        metric_oob <- check_metric_name(metric_oob)
                        mn <- make.names(colnames(metric_oob))

                        bootstrap <- tibble::tibble(
                            optimal_cutpoint =  optcut_b$optimal_cutpoint,
                            AUC_b             =  auc_b,
                            AUC_oob           =  auc_oob
                        )
                        bootstrap <- add_list(bootstrap,
                                              metric_b[, mn],
                                              paste0(mn, "_b"))
                        bootstrap <- add_list(bootstrap,
                                              metric_oob[, mn],
                                              paste0(mn, "_oob"))
                        bootstrap <- add_list(bootstrap,
                                              Acc_b, "acc_b")
                        bootstrap <- add_list(bootstrap,
                                              Acc_oob, "acc_oob")
                        bootstrap <- add_list(bootstrap,
                                              Sens_Spec_b[, "sensitivity"],
                                              "sensitivity_b")
                        bootstrap <- add_list(bootstrap,
                                              Sens_Spec_oob[, "sensitivity"],
                                              "sensitivity_oob")
                        bootstrap <- add_list(bootstrap,
                                              Sens_Spec_b[, "specificity"],
                                              "specificity_b")
                        bootstrap <- add_list(bootstrap,
                                              Sens_Spec_oob[, "specificity"],
                                              "specificity_oob")
                        bootstrap <- add_list(bootstrap,
                                              kap_b[, "cohens_kappa"],
                                              "kappa_b")
                        bootstrap <- add_list(bootstrap,
                                              kap_oob[, "cohens_kappa"],
                                              "kappa_oob")
                        bootstrap <- add_list(bootstrap,
                                              optcut_b$roc_curve[[1]]$tp[opt_ind_b],
                                              "TP_b")
                        bootstrap <- add_list(bootstrap,
                                              optcut_b$roc_curve[[1]]$fp[opt_ind_b],
                                              "FP_b")
                        bootstrap <- add_list(bootstrap,
                                              optcut_b$roc_curve[[1]]$tn[opt_ind_b],
                                              "TN_b")
                        bootstrap <- add_list(bootstrap,
                                              optcut_b$roc_curve[[1]]$fn[opt_ind_b],
                                              "FN_b")
                        bootstrap <- add_list(bootstrap,
                                              roc_curve_oob$tp[opt_ind_oob],
                                              "TP_oob")
                        bootstrap <- add_list(bootstrap,
                                              roc_curve_oob$fp[opt_ind_oob],
                                              "FP_oob")
                        bootstrap <- add_list(bootstrap,
                                              roc_curve_oob$tn[opt_ind_oob],
                                              "TN_oob")
                        bootstrap <- add_list(bootstrap,
                                              roc_curve_oob$fn[opt_ind_oob],
                                              "FN_oob")
                        bootstrap$roc_curve_b =  optcut_b$roc_curve
                        roc_curve_oob <- tidyr::nest_(roc_curve_oob,
                                                      key_col = "roc_curve_oob")
                        bootstrap <- dplyr::bind_cols(bootstrap, roc_curve_oob)
                        return(bootstrap)
                    }
                return(boot_g)
            }))
    }
    res <- dplyr::bind_cols(optcut, boot = bootstrap)
    class(res) <- c("cutpointr", class(res))
    return(res)
}


#' Calculate optimal cutpoints and further statistics for multiple predictors
#'
#' Runs \code{cutpointr_} over multiple predictor variables. By default, \code{cutpointr_}
#' will be run using all columns in the data set as predictors except for the
#' variable in \code{class}.
#'
#' The automatic determination of positive / negative classes and \code{direction}
#' will be carried out separately for every predictor variable. That way, if
#' \code{direction} and the classes are not specified, the reported AUC for every
#' variable will be >= 0.5. AUC may be < 0.5 if subgroups are specified as
#' \code{direction} is equal within every subgroup.
#'
#' @param data A data frame.
#' @param x Character vector of predictor variables.
#' @param class The name of the outcome / independent variable.
#' @param silent Whether to suppress messages.
#' @param ... Further arguments to be passed to cutpointr.
#' @examples
#' library(cutpointr)
#'
#' multi_cutpointr(suicide, x = c("age", "dsi"), class = "suicide",
#'                 pos_class = "yes")
#'
#' multi_cutpointr(suicide, x = c("age", "dsi"), class = "suicide",
#'                 subgroup = "gender", pos_class = "yes")
#'
#' @return A data frame.
#' @importFrom purrr %>%
#' @family main cutpointr functions
#' @export
multi_cutpointr <- function(data, x = colnames(data)[colnames(data) != class],
                            class, silent = FALSE, ...) {
    if(!(is.character(class) & length(class == 1))) {
        stop("class should be the name of the outcome variable (character)")
    }
    args <- list(...)
    if ("subgroup" %in% names(args)) {
        x <- x[x != args$subgroup]
    }
    res <- purrr::map_df(x, function(coln) {
        if (!silent) message(paste0(coln, ":"))
        cutpointr_(data, coln, class, silent = silent, ...) %>%
            dplyr::mutate(variable = coln)
    })
    res <- res[, c("variable", colnames(res)[colnames(res) != "variable"])]
    class(res) <- c("multi_cutpointr", class(res))
    return(res)
}