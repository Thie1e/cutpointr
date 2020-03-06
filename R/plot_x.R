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
#' @family cutpointr plotting functions
#' @export
plot_x <- function(x, display_cutpoint = TRUE, ...) {

    args <- list(...)
    predictor <- as.character(x$predictor[1])
    outcome <- as.character(x$outcome[1])

    if (!(has_column(x, "subgroup"))) {
        res_unnested <- x %>%
            dplyr::select(.data$data) %>%
            tidyr::unnest(.data$data)
        transparency <- 1

        if (all(na_inf_omit(unlist(dplyr::select(res_unnested, predictor))) %% 1 == 0) |
            only_one_unique(
                na_inf_omit(unlist(dplyr::select(res_unnested, predictor)))
            )) {
            all_integer = TRUE
            dist_plot <- ggplot2::geom_bar(alpha = transparency, position = "identity")
        } else {
            all_integer = FALSE
            dist_plot <- ggplot2::geom_density(alpha = transparency)
        }
        dist <- ggplot2::ggplot(res_unnested,
                                ggplot2::aes(x = !!rlang::ensym(predictor))) +
            dist_plot +
            # facet by class because always 2
            ggplot2::facet_wrap(outcome, scales = "free_y") +
            ggplot2::ggtitle("Independent variable",
                             "optimal cutpoint and distribution by class") +
            ggplot2::xlab("value")
        if (display_cutpoint) {
            cutpoint_dat <- x %>%
                dplyr::select(.data$optimal_cutpoint)
            if (is.list(x$optimal_cutpoint)) {
                cutpoint_dat <- tidyr::unnest(cols = optimal_cutpoint,
                                              data = cutpoint_dat)
            }
            dist <- dist +
                ggplot2::geom_vline(data = cutpoint_dat,
                                    mapping = ggplot2::aes(xintercept = optimal_cutpoint),
                                    show.legend = FALSE)
        }
    } else if (has_column(x, "subgroup")) {
        res_unnested <- x %>%
            dplyr::select("data", "subgroup") %>%
            tidyr::unnest(.data$data)
        res_unnested <- dplyr::full_join(res_unnested,
                                         x[, c("optimal_cutpoint", "subgroup")],
                                         by = "subgroup")
        transparency <- 0.6
        if (all(na_inf_omit(unlist(dplyr::select(res_unnested, predictor))) %% 1 == 0) |
            only_one_unique(
                na_inf_omit(unlist(dplyr::select(res_unnested, predictor)))
            )) {
            all_integer = TRUE
            dist_plot <- ggplot2::geom_bar(alpha = transparency, position = "identity")
        } else {
            all_integer = FALSE
            dist_plot <- ggplot2::geom_density(alpha = transparency)
        }
        dist <- ggplot2::ggplot(res_unnested,
                                ggplot2::aes(x = !!rlang::ensym(predictor),
                                             fill = subgroup,
                                             color = subgroup)) +
            dist_plot +
            # facet by class because always 2
            ggplot2::facet_wrap(outcome, scales = "free_y") +
            ggplot2::ggtitle("Independent variable",
                             "optimal cutpoint and distribution by class") +
            ggplot2::xlab("value") +
            ggplot2::labs(color = "Subgroup", fill = "Subgroup")
        if (display_cutpoint) {
            cutpoint_dat <- x %>%
                dplyr::select(.data$subgroup, .data$optimal_cutpoint)
            if (is.list(x$optimal_cutpoint)) {
                cutpoint_dat <- tidyr::unnest(cols = optimal_cutpoint,
                                              data = cutpoint_dat)
            }
            dist <- dist +
                ggplot2::geom_vline(data = cutpoint_dat,
                                    mapping = ggplot2::aes(xintercept = optimal_cutpoint,
                                                           color = subgroup),
                                    show.legend = FALSE)
        }
    }
    if (!all_integer) dist <- dist + ggplot2::geom_rug(alpha = 0.5)
    return(dist)
}
