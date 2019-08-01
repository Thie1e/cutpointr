#' Precision recall plot from a cutpointr object
#'
#' Given a \code{cutpointr} object this function plots the precision recall curve(s)
#' per subgroup, if given.
#' @param x A cutpointr object.
#' @param display_cutpoint (logical) Whether or not to display the optimal
#' cutpoint as a dot on the precision recall curve.
#' @param ... Additional arguments (unused).
#' @examples
#' library(cutpointr)
#'
#' ## Optimal cutpoint for dsi
#' data(suicide)
#' opt_cut <- cutpointr(suicide, dsi, suicide)
#' plot_precision_recall(opt_cut)
#' @family cutpointr plotting functions
#' @export
plot_precision_recall <- function(x, display_cutpoint = TRUE, ...) {

    stopifnot("cutpointr" %in% class(x))
    args <- list(...)

    if (!(has_column(x, "subgroup"))) {
        dts_pr <- "roc_curve"
        transparency <- 1
    } else {
        dts_pr <- c("roc_curve", "subgroup")
        transparency <- 0.6
    }

    if (!(has_column(x, "subgroup"))) {
        plot_title <- ggplot2::ggtitle("Precision recall plot")
    } else {
        plot_title <- ggplot2::ggtitle("Precision recall plot", "by class")
    }
    for (r in 1:nrow(x)) {
        x$roc_curve[[r]] <- x$roc_curve[[r]] %>%
            dplyr::mutate(Precision = tp / (tp + fp),
                           Recall = tp / (tp + fn))
    }
    if (display_cutpoint) {
        optcut_coords <- apply(x, 1, function(r) {
            opt_ind <- get_opt_ind(roc_curve = r$roc_curve,
                                   oc = r$optimal_cutpoint,
                                   direction = r$direction)
            data.frame(Precision = r$roc_curve$Precision[opt_ind],
                       Recall = r$roc_curve$Recall[opt_ind])
        })
        optcut_coords <- do.call(rbind, optcut_coords)
    }
    res_unnested <- x %>%
        dplyr::select(dts_pr) %>%
        tidyr::unnest(.data$roc_curve)
    res_unnested <- res_unnested[is.finite(res_unnested$x.sorted), ]
    if (!(has_column(x, "subgroup"))) {
        pr <- ggplot2::ggplot(res_unnested,
                              ggplot2::aes(x = Recall, y = Precision))
    } else {
        pr <- ggplot2::ggplot(res_unnested,
                              ggplot2::aes(x = Recall, y = Precision,
                                           color = subgroup))
    }
    pr <- pr +
        ggplot2::geom_line() +
        plot_title +
        ggplot2::theme(aspect.ratio = 1) +
        ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))
    if (display_cutpoint) {
        pr <- pr + ggplot2::geom_point(data = optcut_coords, color = "black")
    }

    return(pr)
}
