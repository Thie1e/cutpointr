#' Plot the distribution of the independent variable and the optimal cutpoints
#'
#' Given a cutpointr object this function plots the distribution(s) of the
#' independent variable(s) and the respective cutpoints.
#' @param x A cutpointr object.
#' @param ... Additional arguments (unused).
#' @export
plot_x <- function(x, ...) {

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
    if (all(stats::na.omit(dplyr::select_(res_unnested, .dots = predictor) %% 1 == 0))) {
        dist_plot <- ggplot2::geom_histogram(alpha = 1, position = "dodge",
                                             bins = 30)
    } else {
        dist_plot <- ggplot2::geom_density(alpha = 1)
    }
    dist <- ggplot2::ggplot(res_unnested,
                            ggplot2::aes_string(x = predictor,
                                                fill = fll, color = clr)) +
        dist_plot +
        ggplot2::geom_rug(alpha = 0.5) +
        ggplot2::geom_vline(ggplot2::aes_(xintercept = ~ optimal_cutpoint,
                                          color = col),
                            show.legend = FALSE) +
        # facet by class because always 2
        ggplot2::facet_wrap(outcome, scales = "free_y") +
        ggplot2::ggtitle("Independent variable",
                         "optimal cutpoint and distribution by class") +
        ggplot2::xlab("value")

    return(dist)
}
