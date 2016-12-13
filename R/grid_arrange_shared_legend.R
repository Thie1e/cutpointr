#' @source Forked from https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
grid_arrange_shared_legend <- function(plots, ncol = length(plots), nrow = 1,
                                       position = c("bottom", "right")) {
    # plots <- list(...) ### Changed from original. Instead plots argument.
    position <- match.arg(position)
    g <- ggplot2::ggplotGrob(plots[[1]] + ggplot2::theme(legend.position = position))$grobs
    gb <- sapply(g, function(x) x$name) == "guide-box"
    # No legend because only one class:
    if (!all(!gb)) {
        legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
        lheight <- sum(legend$height)
        lwidth <- sum(legend$width)
    }
    gl <- lapply(plots, function(x) x + ggplot2::theme(legend.position="none"))
    if (!all(!gb)) gl <- c(gl, ncol = ncol, nrow = nrow)

    if (!all(!gb)) {
        if (position == "bottom") {
            hei <- grid::unit.c(grid::unit(1, "npc") - lheight, lheight)
            combined <- gridExtra::arrangeGrob(do.call(gridExtra::arrangeGrob, gl),
                                               legend, ncol = 1,
                                               heights = hei)
        } else if (position == "right") {
            wid <- grid::unit.c(grid::unit(1, "npc") - lwidth, lwidth)
            combined <- gridExtra::arrangeGrob(do.call(gridExtra::arrangeGrob, gl),
                                               legend, ncol = 2,
                                               widths = wid)
        }
    } else {
        # No legend because only one class:
        combined <- gridExtra::arrangeGrob(do.call(arrangeGrob, gl),
                                           nrow = 1, ncol = 1)
    }
    grid::grid.newpage()
    grid::grid.draw(combined)
}
