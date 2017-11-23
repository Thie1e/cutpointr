#' Plot cutpointr objects
#'
#' The plot layout depends on whether subgroups were defined and whether
#' bootstrapping was run.
#'
#' The \code{...} argument can be used to apply \pkg{ggplot2} functions to every individual
#' plot, for example for changing the theme.
#'
#' @param x A cutpointr object.
#' @param ... Further arguments.
#' @examples
#' opt_cut <- cutpointr(suicide, dsi, suicide, gender)
#' plot(opt_cut)
#' plot(opt_cut, ggplot2::theme_bw())
#' @export
plot.cutpointr <- function(x, ...) {
    args <- list(...)

    dist <- plot_x(x)
    roc <- plot_roc(x)
    boot_flag <- !is.null(suppressWarnings(x$boot))
    if (boot_flag) {
        boot_cut <- plot_cut_boot(x)
        boot_metric <- plot_metric_boot(x)
    } else {
        boot_cut    <- NULL
        boot_metric <- NULL
    }

    #
    # Compose plots
    #
    plots <- list(dist, roc, boot_cut, boot_metric)
    keep <- !(purrr::map_lgl(plots, is.null))
    plots <- plots[keep]
    plots <- lapply(plots, function(p) p + args)
    rows <- round(sum(keep) / 2)
    pos <- ifelse(rows > 1, "right", "bottom")
    suppressMessages(p <- grid_arrange_shared_legend(plots,
                                                nrow = rows, ncol = 2,
                                                position = pos))
    invisible(p)
}
