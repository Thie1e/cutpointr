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

    if (!("cutpointr" %in% class(x))) {
        stop("Only cutpointr objects are supported.")
    }
    args <- list(...)

    if (!(has_column(x, "subgroup"))) {
        dts_pr <- "roc_curve"
        transparency <- 1
    } else {
        dts_pr <- c("roc_curve", "subgroup")
        transparency <- 0.6
    }

    plot_title <- ggplot2::ggtitle("Precision Recall Plot")
    for (r in 1:nrow(x)) {
        x$roc_curve[[r]] <- x$roc_curve[[r]] %>%
            dplyr::mutate(Precision = tp / (tp + fp),
                           Recall = tp / (tp + fn))
    }
    if (display_cutpoint) {
        optcut_coords <- purrr::pmap_df(x, function(...) {
            args <- list(...)
            opt_ind <- get_opt_ind(roc_curve = args$roc_curve,
                                   oc = args$optimal_cutpoint,
                                   direction = args$direction)
            data.frame(Precision = args$roc_curve$Precision[opt_ind],
                       Recall = args$roc_curve$Recall[opt_ind])

        })
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
