#' Plot the bootstrapped metric distribution
#'
#' Given a cutpointr object this function plots the bootstrapped metric distribution.
#' The metric depends on the function that was supplied to `metric` in the
#' call to cutpointr.
#' The cutpointr function has to be run with boot_runs` > 0 to enable bootstrapping.
#' @param x A cutpointr object
#' @param ... Additional arguments (unused)
#' @export
plot_metric_boot <- function(x, ...) {

    args <- list(...)

    if (is.null(suppressWarnings(x$subgroup))) {
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

    if(!is.null(suppressWarnings(x$boot))) {
        res_boot_unnested <- x %>%
            dplyr::select_(.dots = dts_boot) %>%
            tidyr::unnest_(unnest_cols = "boot")
        met_ind <- which(colnames(res_boot_unnested) == "optimal_cutpoint") + 1
        metric_name <- colnames(res_boot_unnested)[met_ind]
        if (all(stats::na.omit(get(metric_name, res_boot_unnested) %% 1 == 0))) {
            dist_plot <- ggplot2::geom_histogram(alpha = 1, bins = 30,
                                                 position = "dodge")
        } else {
            dist_plot <- ggplot2::geom_density(alpha = transparency)
        }
        boot_metric <- suppressMessages(
            ggplot2::ggplot(res_boot_unnested,
                            ggplot2::aes_string(x = metric_name,
                                                fill = fll, color = clr)) +
                dist_plot +
                ggplot2::geom_rug(alpha = 0.5) +
                ggplot2::ggtitle("Bootstrap",
                                 paste("out-of-bag estimates of", metric_name)) +
                ggplot2::xlab(metric_name)
        )
    } else {
        stop("No bootstrap results found. Was boot_runs > 0 in cutpointr?")
    }

    return(boot_metric)
}
