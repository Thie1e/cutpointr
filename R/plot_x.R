#' Plot the distribution of the independent variable per class from a cutpointr object
#'
#' Given a \code{cutpointr} object this function plots the distribution(s) of the
#' independent variable(s) and the respective cutpoints per class.
#' @param x A cutpointr object.
#' @param display_cutpoint (logical) Whether or not to display the optimal
#' cutpoint as a vertical line.
#' @param ... Additional arguments (unused).
#' @examples
#' opt_cut <- cutpointr(suicide, dsi, suicide)
#' plot_x(opt_cut)
#'
#' ## With subgroup
#' opt_cut_2groups <- cutpointr(suicide, dsi, suicide, gender)
#' plot_x(opt_cut_2groups)
#' @export
plot_x <- function(x, display_cutpoint = TRUE, ...) {

    args <- list(...)
    predictor <- as.name(x$predictor[1])
    outcome <- as.name(x$outcome[1])

    if (is.null(suppressWarnings(x$subgroup))) {
        dts <- "data"
        fll <- NULL
        clr <- NULL
        clr_roc <- NULL
        transparency <- 1
    } else {
        dts <- c("data", "subgroup")
        fll <- "subgroup"
        clr <- "subgroup"
        clr_roc <- ~ subgroup
        transparency <- 0.6
    }

    res_unnested <- x %>%
        dplyr::select_(.dots = dts) %>%
        tidyr::unnest_(unnest_cols = "data")
    if (is.null(suppressWarnings(x$subgroup))) {
        res_unnested$optimal_cutpoint <- x$optimal_cutpoint
        col <- NULL
    } else {
        res_unnested <- dplyr::full_join(res_unnested,
                                         x[, c("optimal_cutpoint", "subgroup")],
                                         by = "subgroup")
        col <- ~ subgroup
    }
    if (all(na_inf_omit(unlist(dplyr::select_(res_unnested, .dots = predictor))) %% 1 == 0) |
        only_one_unique(
            na_inf_omit(unlist(dplyr::select_(res_unnested, .dots = predictor)))
        )) {
        all_integer = TRUE
        dist_plot <- ggplot2::geom_bar(alpha = transparency, position = "identity")
    } else {
        all_integer = FALSE
        dist_plot <- ggplot2::geom_density(alpha = transparency)
    }
    dist <- ggplot2::ggplot(res_unnested,
                            ggplot2::aes_string(x = predictor,
                                                fill = fll, color = clr)) +
        dist_plot +
        # facet by class because always 2
        ggplot2::facet_wrap(outcome, scales = "free_y") +
        ggplot2::ggtitle("Independent variable",
                         "optimal cutpoint and distribution by class") +
        ggplot2::xlab("value")
    if (display_cutpoint) dist <- dist +
        ggplot2::geom_vline(ggplot2::aes_(xintercept = ~ optimal_cutpoint,
                                          color = col), show.legend = FALSE)

    if (!all_integer) dist <- dist + ggplot2::geom_rug(alpha = 0.5)
    return(dist)
}
