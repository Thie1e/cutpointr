#' Plot ROC curve from a cutpointr object
#'
#' Given a cutpointr object this function plots the ROC curve(s)
#' per subgroup, if given.
#' @param x A cutpointr object.
#' @param display_cutpoint (logical) Whether or not to display the optimal
#' cutpoint as a dot on the ROC curve.
#' @param ... Additional arguments (unused).
#' @examples
#' opt_cut <- cutpointr(suicide, dsi, suicide)
#' plot_roc(opt_cut, display_cutpoint = FALSE)
#'
#' opt_cut_2groups <- cutpointr(suicide, dsi, suicide, gender)
#' plot_roc(opt_cut_2groups, display_cutpoint = TRUE)
#' @export
plot_roc <- function(x, display_cutpoint = TRUE, ...) {

    stopifnot("cutpointr" %in% class(x))
    args <- list(...)
    predictor <- as.name(x$predictor[1])
    outcome <- as.name(x$outcome[1])

    if (is.null(suppressWarnings(x$subgroup))) {
        dts_roc <- "roc_curve"
        fll <- NULL
        clr <- NULL
        clr_roc <- NULL
        transparency <- 1
    } else {
        dts_roc <- c("roc_curve", "subgroup")
        fll <- "subgroup"
        clr <- "subgroup"
        clr_roc <- ~ subgroup
        transparency <- 0.6
    }

    if (suppressWarnings(is.null(x$subgroup))) {
        roc_title <- ggplot2::ggtitle("ROC curve")
    } else {
        roc_title <- ggplot2::ggtitle("ROC curve", "by class")
    }
    if (display_cutpoint) {
        optcut_coords <- apply(x, 1, function(r) {
            opt_ind <- get_opt_ind(roc_curve = r$roc_curve,
                                   oc = r$optimal_cutpoint,
                                   direction = r$direction)
            data.frame(tpr = r$roc_curve$tpr[opt_ind],
                       tnr = r$roc_curve$tnr[opt_ind])
        })
        optcut_coords <- do.call(rbind, optcut_coords)
    }
    res_unnested <- x %>%
        dplyr::select_(.dots = dts_roc) %>%
        tidyr::unnest_(unnest_cols = "roc_curve")
    roc <- ggplot2::ggplot(res_unnested,
                           ggplot2::aes_(x = ~ 1 - tnr, y = ~ tpr, color = clr_roc)) +
        ggplot2::geom_step() +
        roc_title +
        ggplot2::xlab("1 - Specificity") +
        ggplot2::ylab("Sensitivity") +
        ggplot2::theme(aspect.ratio = 1)
    if (display_cutpoint) {
        roc <- roc + ggplot2::geom_point(data = optcut_coords, color = "black")
    }
    return(roc)
}