#' Plot a metric over all possible cutoffs
#'
#' If maximize_metric is used as method function in cutpointr the computed
#' metric values over all possible cutoffs can be plotted. Generally, this
#' works for method functions that return a ROC-curve including the metric
#' value for every cutpoint along with the optimal cutpoint.
#'
#' @param x A cutpointr object.
#' @param conf_lvl The confidence level of the bootstrap confidence interval.
#' Set to 0 to draw no bootstrap confidence interval.
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
#' @importFrom dplyr %>%
#' @export
plot_metric <- function(x, conf_lvl = 0.95) {
    stopifnot("cutpointr" %in% class(x))
    if (suppressWarnings(is.null(x$roc_curve[[1]]$m))) {
        stop(paste("The cutpointr object does not include a metric column in",
                   "roc_curve - maybe because a method other than",
                   "maximize_metric or minimize_metric was used"))
    }

    if ("boot" %in% colnames(x)) {
        if ("subgroup" %in% colnames(x)) {
            roc_b_unnested <- x %>%
                dplyr::select_(.dots = c("boot", "subgroup")) %>%
                tidyr::unnest_(unnest_cols = "boot") %>%
                dplyr::select_(.dots = c("subgroup", "roc_curve_b")) %>%
                tidyr::unnest_(unnest_cols = "roc_curve_b")
            roc_b_unnested <- roc_b_unnested[is.finite(roc_b_unnested$x.sorted), ]
            roc_b_unnested <- roc_b_unnested %>%
                dplyr::select_(.dots = c("x.sorted", "m", "subgroup")) %>%
                dplyr::group_by_(.dots = c("x.sorted", "subgroup")) %>%
                dplyr::summarise_(ymin = ~ quantile(m, (1 - conf_lvl) / 2),
                                  ymax = ~ quantile(m, 1 - (1 - conf_lvl) / 2))
        } else {
            roc_b_unnested <- x$boot[[1]] %>%
                tidyr::unnest_(unnest_cols = "roc_curve_b")
            roc_b_unnested <- roc_b_unnested[is.finite(roc_b_unnested$x.sorted), ]
            roc_b_unnested <- roc_b_unnested %>%
                dplyr::select_(.dots = c("x.sorted", "m")) %>%
                dplyr::group_by_(.dots = "x.sorted") %>%
                dplyr::summarise_(ymin = ~ quantile(m, (1 - conf_lvl) / 2),
                                  ymax = ~ quantile(m, 1 - (1 - conf_lvl) / 2))
        }
    }
    metric_name <- find_metric_name(x)
    if ("subgroup" %in% colnames(x)) {
        res_unnested <- x %>%
            dplyr::select_(.dots = c("roc_curve", "subgroup")) %>%
            tidyr::unnest_(unnest_cols = "roc_curve")
        res_unnested <- res_unnested[is.finite(res_unnested$x.sorted),
                                     c("x.sorted", "m", "subgroup")]
        if ("boot" %in% colnames(x)) {
            res_unnested <- merge(res_unnested,
                                  roc_b_unnested[, c("subgroup", "x.sorted", "ymin", "ymax")],
                                  by = c("x.sorted", "subgroup"))
            p <- ggplot2::ggplot(res_unnested, ggplot2::aes_(x = ~ x.sorted,
                                                             y = ~ m,
                                                             ymin = ~ ymin,
                                                             ymax = ~ ymax,
                                                             color = ~ subgroup,
                                                             fill = ~ subgroup)) +
                ggplot2::geom_line() +
                ggplot2::geom_point() +
                ggplot2::ggtitle("Metric values by cutpoint value") +
                ggplot2::ylab(metric_name) + ggplot2::xlab("Cutpoint") +
                ggplot2::geom_ribbon(alpha = 0.2)
        } else {
            p <- ggplot2::ggplot(res_unnested, ggplot2::aes_(x = ~ x.sorted,
                                                                   y = ~ m,
                                                                   color = ~ subgroup)) +
                ggplot2::geom_line() + ggplot2::geom_point() +
                ggplot2::ggtitle("Metric values by cutpoint value") +
                ggplot2::ylab(metric_name) + ggplot2::xlab("Cutpoint")
        }
    } else {
        res_unnested <- x %>%
            dplyr::select_(.dots = "roc_curve") %>%
            tidyr::unnest_(unnest_cols = "roc_curve")
        res_unnested <- res_unnested[is.finite(res_unnested$x.sorted),
                                     (c("x.sorted", "m"))]
        if ("boot" %in% colnames(x)) {
            res_unnested <- merge(res_unnested,
                                  roc_b_unnested[, c("x.sorted", "ymin", "ymax")],
                                  by = "x.sorted")
            p <- ggplot2::ggplot(res_unnested, ggplot2::aes_(x = ~ x.sorted,
                                                             y = ~ m,
                                                             ymax = ~ ymax,
                                                             ymin = ~ ymin)) +
                ggplot2::geom_line() + ggplot2::geom_point() +
                ggplot2::ggtitle("Metric values by cutpoint value") +
                ggplot2::ylab(metric_name) + ggplot2::xlab("Cutpoint") +
                ggplot2::geom_ribbon(alpha = 0.2)
        } else {
            p <- ggplot2::ggplot(res_unnested, ggplot2::aes_(x = ~ x.sorted,
                                                             y = ~ m)) +
                ggplot2::geom_line() + ggplot2::geom_point() +
                ggplot2::ggtitle("Metric values by cutpoint value") +
                ggplot2::ylab(metric_name) + ggplot2::xlab("Cutpoint")
        }
    }
    return(p)
}

