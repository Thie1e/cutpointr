#' Precision recall plot from a cutpointr object
#'
#' Given a cutpointr object this function plots the precision recall curve(s)
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
#' @export
plot_precision_recall <- function(x, display_cutpoint = TRUE, ...) {

    stopifnot("cutpointr" %in% class(x))
    args <- list(...)

    if (is.null(suppressWarnings(x$subgroup))) {
        dts_pr <- "roc_curve"
        fll <- NULL
        clr <- NULL
        clr_pr <- NULL
        transparency <- 1
    } else {
        dts_pr <- c("roc_curve", "subgroup")
        fll <- "subgroup"
        clr <- "subgroup"
        clr_pr <- ~ subgroup
        transparency <- 0.6
    }

    if (suppressWarnings(is.null(x$subgroup))) {
        plot_title <- ggplot2::ggtitle("Precision recall plot")
    } else {
        plot_title <- ggplot2::ggtitle("Precision recall plot", "by class")
    }
    for (r in 1:nrow(x)) {
        x$roc_curve[[r]] <- x$roc_curve[[r]] %>%
            dplyr::mutate_(Precision = ~ tp / (tp + fp),
                           Recall = ~ tp / (tp + fn))
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
        dplyr::select_(.dots = dts_pr) %>%
        tidyr::unnest_(unnest_cols = "roc_curve")
    # Drop possible NaN at x.sorted = Inf or -Inf
    res_unnested <- stats::na.omit(res_unnested)
    pr <- ggplot2::ggplot(res_unnested,
                          ggplot2::aes_(x = ~ Recall, y = ~ Precision, color = clr_pr)) +
        ggplot2::geom_line() +
        plot_title +
        ggplot2::xlab("Recall") +
        ggplot2::ylab("Precision") +
        ggplot2::theme(aspect.ratio = 1) +
        ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))
    if (display_cutpoint) {
        pr <- pr + ggplot2::geom_point(data = optcut_coords, color = "black")
    }

    return(pr)
}
