optimize_metric <- function(data, x, class, metric_func = youden,
                            pos_class = NULL, neg_class = NULL, minmax,
                            direction, metric_name = "metric") {
    metric_name_call <- as.character(substitute(metric_func))
    if (metric_name_call != "metric_func") metric_name <- metric_name_call
    roccurve <- roc(data = data, x = x, class = class, pos_class = pos_class,
                    neg_class = neg_class, direction = direction)
    m <- metric_func(tp = roccurve[, "tp"], fp = roccurve[, "fp"],
                     tn = roccurve[, "tn"], fn = roccurve[, "fn"])
    roccurve$m <- as.numeric(m)
    if (!is.null(colnames(m))) metric_name <- colnames(m)
    if (minmax == "max") {
        max_m <- max(m)
        opt <- m == max_m
        oc <- min(roccurve[, "x.sorted"][opt])
        if (sum(opt) > 1) {
            warning(paste("Multiple optimal cutpoints found, returning minimum of:",
                          paste(roccurve[, "x.sorted"][opt], collapse = ", ")))
        }
        m_oc <- max_m
    } else if (minmax == "min") {
        min_m <- min(m)
        opt <- m == min_m
        oc <- max(roccurve[, "x.sorted"][opt])
        if (sum(opt) > 1) {
            warning(paste("Multiple optimal cutpoints found, returning maximum of:",
                          paste(roccurve[, "x.sorted"][opt], collapse = ", ")))
        }
        m_oc <- min_m
    }
    res <- tidyr::nest_(roccurve, key_col = "roc_curve",
                        nest_cols = colnames(roccurve))
    res$optimal_cutpoint <- oc
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
#' @export
maximize_metric <- function(data, x, class, metric_func = youden,
                            pos_class = NULL, neg_class = NULL,
                            direction) {
    metric_name <- as.character(substitute(metric_func))
    optimize_metric(data = data, x = x, class = class, metric_func = metric_func,
                    pos_class = pos_class, neg_class = neg_class, minmax = "max",
                    direction = direction, metric_name = metric_name)
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
#' @export
minimize_metric <- function(data, x, class, metric_func = youden,
                            pos_class = NULL, neg_class = NULL,
                            direction) {
    metric_name <- as.character(substitute(metric_func))
    optimize_metric(data = data, x = x, class = class, metric_func = metric_func,
                    pos_class = pos_class, neg_class = neg_class, minmax = "min",
                    direction = direction, metric_name = metric_name)
}



