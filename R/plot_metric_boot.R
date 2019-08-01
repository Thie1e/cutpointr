#' Plot the bootstrapped metric distribution from a cutpointr object
#'
#' Given a \code{cutpointr} object this function plots the bootstrapped metric distribution,
#' i.e. the distribution of out-of-bag metric values.
#' The metric depends on the function that was supplied to \code{metric} in the
#' call to \code{cutpointr}.
#' The \code{cutpointr} function has to be run with \code{boot_runs}` > 0 to enable bootstrapping.
#' @param x A cutpointr object.
#' @param ... Additional arguments (unused)
#' @examples
#' set.seed(300)
#' opt_cut <- cutpointr(suicide, dsi, suicide, boot_runs = 10)
#' plot_metric_boot(opt_cut)
#' @family cutpointr plotting functions
#' @importFrom rlang .data
#' @export
plot_metric_boot <- function(x, ...) {

    args <- list(...)

    if (!(has_column(x, "subgroup"))) {
        dts_boot <- "boot"
        dts <- "data"
        transparency <- 1
    } else {
        dts_boot <- c("boot", "subgroup")
        dts <- c("data", "subgroup")
        transparency <- 0.6
    }

    if (has_boot_results(x)) {
        res_boot_unnested <- x %>%
            dplyr::select(dts_boot) %>%
            dplyr::mutate(boot = prepare_bind_rows(.data$boot)) %>%
            tidyr::unnest(.data$boot) %>%
            dplyr::select(-c("roc_curve_b", "roc_curve_oob"))
        # If multiple optimal cutpoints optimal_cutpoint is a list
        if (is.list(res_boot_unnested$optimal_cutpoint)) {
            res_boot_unnested <- res_boot_unnested %>%
                tidyr::unnest()
        }
        metric_name <- find_metric_name_boot(res_boot_unnested)
        if (all(na_inf_omit(get(metric_name, res_boot_unnested) %% 1 == 0)) |
            only_one_unique(na_inf_omit(get(metric_name, res_boot_unnested)))) {
            all_integer = TRUE
            dist_plot <- ggplot2::geom_bar(alpha = 1, position = "dodge")
        } else {
            all_integer = FALSE
            dist_plot <- ggplot2::geom_density(alpha = transparency)
        }
        if (!(has_column(x, "subgroup"))) {
            boot_metric <- suppressMessages(
                ggplot2::ggplot(res_boot_unnested,
                                ggplot2::aes(x = !!rlang::ensym(metric_name))) +
                    dist_plot +
                    ggplot2::ggtitle("Bootstrap", "out-of-bag estimates") +
                    ggplot2::xlab(metric_name)
            )
        } else {
            boot_metric <- suppressMessages(
                ggplot2::ggplot(res_boot_unnested,
                                ggplot2::aes(x = !!rlang::ensym(metric_name),
                                             fill = subgroup,
                                             color = subgroup)) +
                    dist_plot +
                    ggplot2::ggtitle("Bootstrap", "out-of-bag estimates") +
                    ggplot2::xlab(metric_name)
            )
        }
        if (!all_integer) boot_metric <- boot_metric +
            ggplot2::geom_rug(alpha = 0.5)
    } else {
        stop("No bootstrap results found. Was boot_runs > 0 in cutpointr?")
    }

    return(boot_metric)
}
