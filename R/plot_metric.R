#' Plot a metric over all possible cutoffs
#'
#' If maximize_metric is used as method function in cutpointr the computed
#' metric values over all possible cutoffs can be plotted. Generally, this
#' works for method functions that return a ROC-curve including the metric
#' value for every cutpoint along with the optimal cutpoint.
#'
#' @param x A cutpointr object.
#' @examples
#' set.seed(123)
#' class_a <- rnorm(50, mean = 10)
#' set.seed(234)
#' class_b <- rnorm(50, mean = 11)
#' dat <- data.frame(x = c(class_a, class_b), y = c(rep(0, 50), c(rep(1, 50))))
#' opt_cut <- cutpointr(dat, x, y)
#' plot_metric(opt_cut)
#'
#' ## The same as:
#' opt_cut <- cutpointr(dat, x, y, method = maximize_metric, metric = youden,
#'   pos_class = 1, direction = ">=")
#' plot_metric(opt_cut)
#' @export
plot_metric <- function(x) {
    stopifnot("cutpointr" %in% class(x))
    if (suppressWarnings(is.null(x$roc_curve[[1]]$m))) {
        stop(paste("The cutpointr object does not include a metric column in",
                   "roc_curve - maybe because a method other than",
                   "maximize_metric or minimize_metric was used"))
    }
    metric_name <- find_metric_name(x)
    if ("subgroup" %in% colnames(x)) {
        res_unnested <- x %>%
            dplyr::select_(.dots = c("roc_curve", "subgroup")) %>%
            tidyr::unnest_(unnest_cols = "roc_curve")
        p <- ggplot2::ggplot(res_unnested, ggplot2::aes_(x = ~ x.sorted,
                                                         y = ~ m,
                                                         color = ~ subgroup)) +
            ggplot2::geom_line() + ggplot2::geom_point() +
            ggplot2::ggtitle("Metric values by cutpoint value") +
            ggplot2::ylab(metric_name) + ggplot2::xlab("Cutpoint")
    } else {
        res_unnested <- x %>%
            dplyr::select_(.dots = "roc_curve") %>%
            tidyr::unnest_(unnest_cols = "roc_curve")
        p <- ggplot2::ggplot(res_unnested, ggplot2::aes_(x = ~ x.sorted,
                                                         y = ~ m)) +
            ggplot2::geom_line() + ggplot2::geom_point() +
            ggplot2::ggtitle("Metric values by cutpoint value") +
            ggplot2::ylab(metric_name) + ggplot2::xlab("Cutpoint")
    }
    return(p)
}