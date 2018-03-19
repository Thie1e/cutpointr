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
#' @export
plot_metric_boot <- function(x, ...) {

    args <- list(...)

    if (!(has_column(x, "subgroup"))) {
        dts_boot <- "boot"
        dts <- "data"
        fll <- NULL
        clr <- NULL
        transparency <- 1
    } else {
        dts_boot <- c("boot", "subgroup")
        dts <- c("data", "subgroup")
        fll <- "subgroup"
        clr <- "subgroup"
        transparency <- 0.6
    }

    if(has_column(x, "boot")) {
        res_boot_unnested <- x %>%
            dplyr::select_(.dots = dts_boot) %>%
            dplyr::mutate_(boot = ~ prepare_bind_rows(boot)) %>%
            tidyr::unnest_(unnest_cols = "boot") %>%
            dplyr::select_(.dots = list("-roc_curve_b", "-roc_curve_oob"))
        # If multiple optimal cutpoints optimal_cutpoint is a list
        if (is.list(res_boot_unnested$optimal_cutpoint)) {
            res_boot_unnested <- res_boot_unnested %>%
                tidyr::unnest_()
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
        boot_metric <- suppressMessages(
            ggplot2::ggplot(res_boot_unnested,
                            ggplot2::aes_string(x = metric_name,
                                                fill = fll, color = clr)) +
                dist_plot +
                ggplot2::ggtitle("Bootstrap", "out-of-bag estimates") +
                ggplot2::xlab(metric_name)
        )
        if (!all_integer) boot_metric <- boot_metric +
            ggplot2::geom_rug(alpha = 0.5)
    } else {
        stop("No bootstrap results found. Was boot_runs > 0 in cutpointr?")
    }

    return(boot_metric)
}
