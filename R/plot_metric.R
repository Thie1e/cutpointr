#' Plot a metric over all possible cutoffs from a cutpointr object
#'
#' If \code{maximize_metric} is used as \code{method} function in cutpointr the computed
#' metric values over all possible cutoffs can be plotted. Generally, this
#' works for method functions that return a ROC-curve including the metric
#' value for every cutpoint along with the optimal cutpoint.
#'
#' @param x A cutpointr object.
#' @param conf_lvl The confidence level of the bootstrap confidence interval.
#' Set to 0 to draw no bootstrap confidence interval.
#' @param add_unsmoothed Add the line of unsmoothed metric values to the plot.
#' Applicable for some smoothing methods, e.g. maximize_gam_metric.
#' @examples
#' opt_cut <- cutpointr(suicide, dsi, suicide)
#' plot_metric(opt_cut)
#' @importFrom dplyr %>%
#' @family cutpointr plotting functions
#' @family cutpointr plotting functions
#' @export
plot_metric <- function(x, conf_lvl = 0.95, add_unsmoothed = TRUE) {
    stopifnot("cutpointr" %in% class(x))
    if (!(has_column(x$roc_curve[[1]], "m"))) {
        stop(paste("The cutpointr object does not include a metric column in",
                   "roc_curve - maybe because a method other than",
                   "maximize_metric or minimize_metric was used"))
    }

    if (has_boot_results(x) & conf_lvl != 0) {
        if (has_column(x, "subgroup")) {
            roc_b_unnested <- x %>%
                dplyr::select(c("boot", "subgroup")) %>%
                dplyr::mutate(boot = prepare_bind_rows(.data$boot)) %>%
                tidyr::unnest(.data$boot) %>%
                dplyr::select(c("subgroup", "roc_curve_b")) %>%
                tidyr::unnest(.data$roc_curve_b)
            roc_b_unnested <- roc_b_unnested[is.finite(roc_b_unnested$x.sorted), ]
            roc_b_unnested <- roc_b_unnested %>%
                dplyr::select(c("x.sorted", "m", "subgroup")) %>%
                dplyr::group_by(.data$x.sorted, .data$subgroup) %>%
                dplyr::summarise(ymin = stats::quantile(.data$m, (1 - conf_lvl) / 2, na.rm = TRUE),
                                 ymax = stats::quantile(.data$m, 1 - (1 - conf_lvl) / 2, na.rm = TRUE))
        } else {
            # No subgroups, but bootstrap
            roc_b_unnested <- x[["boot"]][[1]] %>%
                tidyr::unnest(.data$roc_curve_b)
            roc_b_unnested <- roc_b_unnested[is.finite(roc_b_unnested$x.sorted), ]
            roc_b_unnested <- roc_b_unnested %>%
                dplyr::select(c("x.sorted", "m")) %>%
                dplyr::group_by(.data$x.sorted) %>%
                dplyr::summarise(ymin = stats::quantile(.data$m, (1 - conf_lvl) / 2, na.rm = TRUE),
                                 ymax = stats::quantile(.data$m, 1 - (1 - conf_lvl) / 2, na.rm = TRUE))
        }
    }
    metric_name <- find_metric_name(x)
    if ("subgroup" %in% colnames(x)) {
        res_unnested <- x %>%
            dplyr::select(c("roc_curve", "subgroup")) %>%
            tidyr::unnest(.data$roc_curve)
        res_unnested <- res_unnested[is.finite(res_unnested$x.sorted), ]
        if (has_boot_results(x) & conf_lvl != 0) {
            res_unnested <- merge(res_unnested,
                                  roc_b_unnested[, c("subgroup", "x.sorted", "ymin", "ymax")],
                                  by = c("x.sorted", "subgroup"))
            p <- ggplot2::ggplot(res_unnested, ggplot2::aes(x =  x.sorted,
                                                            y =  m,
                                                            ymin =  ymin,
                                                            ymax =  ymax,
                                                            color =  subgroup,
                                                            fill =  subgroup)) +
                ggplot2::geom_line() +
                ggplot2::geom_point() +
                ggplot2::ylab(metric_name) + ggplot2::xlab("Cutpoint") +
                ggplot2::geom_ribbon(alpha = 0.2, size = 0)
        } else {
            p <- ggplot2::ggplot(res_unnested, ggplot2::aes(x = x.sorted,
                                                            y = m,
                                                            color = subgroup)) +
                ggplot2::geom_line() + ggplot2::geom_point() +
                ggplot2::ylab(metric_name) + ggplot2::xlab("Cutpoint")
        }
        if (add_unsmoothed & has_column(res_unnested, "m_unsmoothed")) {
            p <- p +
                ggplot2::geom_line(data = res_unnested, linetype = "dashed",
                                   mapping = ggplot2::aes(x = x.sorted,
                                                          y = m_unsmoothed,
                                                          color = subgroup))
        }
    } else {
        # No subgroups
        res_unnested <- x %>%
            dplyr::select(.data$roc_curve) %>%
            tidyr::unnest(.data$roc_curve)
        res_unnested <- res_unnested[is.finite(res_unnested$x.sorted), ]
        if (has_boot_results(x) & conf_lvl != 0) {
            res_unnested <- merge(res_unnested,
                                  roc_b_unnested[, c("x.sorted", "ymin", "ymax")],
                                  by = "x.sorted")
            p <- ggplot2::ggplot(res_unnested, ggplot2::aes(x = x.sorted,
                                                            y = m,
                                                            ymax = ymax,
                                                            ymin = ymin)) +
                ggplot2::geom_line() + ggplot2::geom_point() +
                ggplot2::ylab(metric_name) + ggplot2::xlab("Cutpoint") +
                ggplot2::geom_ribbon(alpha = 0.2, size = 0)
        } else {
            p <- ggplot2::ggplot(res_unnested, ggplot2::aes(x = x.sorted,
                                                            y = m)) +
                ggplot2::geom_line() + ggplot2::geom_point() +
                ggplot2::ylab(metric_name) + ggplot2::xlab("Cutpoint")
        }
        if (add_unsmoothed & has_column(res_unnested, "m_unsmoothed")) {
            p <- p +
                ggplot2::geom_line(data = res_unnested, linetype = "dashed",
                                   mapping = ggplot2::aes(x = x.sorted,
                                                          y = m_unsmoothed))
        }
    }

    if (add_unsmoothed & has_column(res_unnested, "m_unsmoothed")) {
        p <- p + ggplot2::ggtitle("Metric values by cutpoint value",
                                  "in-sample results, unsmoothed values as dashed line")
    } else {
        p <- p + ggplot2::ggtitle("Metric values by cutpoint value",
                                  "in-sample results")
    }

    return(p)
}

