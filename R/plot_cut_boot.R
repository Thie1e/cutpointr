#' Plot the bootstrapped distribution of optimal cutpoints from a cutpointr object
#'
#' Given a cutpointr object this function plots the bootstrapped distribution
#' of optimal cutpoints. \code{cutpointr} has to be run with \code{boot_runs}` > 0
#' to enable bootstrapping.
#' @param x A cutpointr object.
#' @param ... Additional arguments (unused).
#' @examples
#' set.seed(100)
#' opt_cut <- cutpointr(suicide, dsi, suicide, boot_runs = 10)
#' plot_cut_boot(opt_cut)
#' @family cutpointr plotting functions
#' @export
plot_cut_boot <- function(x, ...) {

    if(!("cutpointr" %in% class(x))) stop("x is no cutpointr object.")
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

    if (has_boot_results(x)) {
        res_boot_unnested <- x %>%
            dplyr::select(dts_boot) %>%
            dplyr::mutate(boot = prepare_bind_rows(.data$boot)) %>%
            tidyr::unnest(.data$boot)
        cutpoints <- unlist(res_boot_unnested$optimal_cutpoint)
        if (all(na_inf_omit(cutpoints %% 1 == 0)) |
            only_one_unique(na_inf_omit(cutpoints))) {
            all_integer = TRUE
            dist_plot <- ggplot2::geom_bar(alpha = transparency,
                                           position = "identity")
        } else {
            all_integer = FALSE
            dist_plot <- ggplot2::geom_density(alpha = transparency)
        }
        # If multiple optimal cutpoints optimal_cutpoint is a list
        if (is.list(res_boot_unnested$optimal_cutpoint)) {
            res_boot_unnested <- res_boot_unnested %>%
                dplyr::select(-c("roc_curve_b", "roc_curve_oob")) %>%
                tidyr::unnest()
        }
        boot_cut <- suppressMessages(
            ggplot2::ggplot(res_boot_unnested,
                            ggplot2::aes_string(x = "optimal_cutpoint",
                                                fill = fll, color = clr)) +
                dist_plot +
                ggplot2::ggtitle("Bootstrap", "distribution of optimal cutpoints") +
                ggplot2::xlab("optimal cutpoint")
        )
        if (!all_integer) boot_cut <- boot_cut + ggplot2::geom_rug(alpha = 0.5)
    } else {
        stop("No bootstrap results found. Was boot_runs > 0 in cutpointr?")
    }

    return(boot_cut)
}
