#' Plot the bootstrapped distribution of optimal cutpoints
#'
#' Given a cutpointr object this function plots the bootstrapped distribution
#' of optimal cutpoints. The cutpointr function has to be run with
#' boot_runs` > 0 to enable bootstrapping.
#' @param x A cutpointr object.
#' @param ... Additional arguments (unused).
#' @export
plot_cut_boot <- function(x, ...) {

    if(!("cutpointr" %in% class(x))) stop("x is no cutpointr object.")
    args <- list(...)

    if (is.null(suppressWarnings(x$subgroup))) {
        dts_boot <- "boot"
        dts <- "data"
        fll <- NULL
        clr <- NULL
        transparency <- 0.6
    } else {
        dts_boot <- c("boot", "subgroup")
        dts <- c("data", "subgroup")
        fll <- "subgroup"
        clr <- "subgroup"
        transparency <- 1
    }

    if(!is.null(suppressWarnings(x$boot))) {
        res_boot_unnested <- x %>%
            dplyr::select_(.dots = dts_boot) %>%
            tidyr::unnest_(unnest_cols = "boot")
        if (all(stats::na.omit(res_boot_unnested$optimal_cutpoint %% 1 == 0))) {
            dist_plot <- ggplot2::geom_histogram(alpha = 1, bins = 30,
                                                 position = "dodge")
        } else {
            dist_plot <- ggplot2::geom_density(alpha = transparency)
        }
        boot_cut <- suppressMessages(
            ggplot2::ggplot(res_boot_unnested,
                            ggplot2::aes_string(x = "optimal_cutpoint",
                                                fill = fll, color = clr)) +
                dist_plot +
                ggplot2::geom_rug(alpha = 0.5) +
                ggplot2::ggtitle("Bootstrap", "distribution of optimal cutpoints") +
                ggplot2::xlab("optimal cutpoint")
        )
    } else {
        stop("No bootstrap results found. Was boot_runs > 0 in cutpointr?")
    }

    return(boot_cut)
}
