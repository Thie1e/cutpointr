#' Sensitivity and specificity plot from a cutpointr object
#'
#' Given a \code{cutpointr} object this function plots the sensitivity and specificity
#' curve(s) per subgroup, if the latter is given.
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
#' plot_sensitivity_specificity(opt_cut)
#' @family cutpointr plotting functions
#' @export
#' @importFrom purrr %>%
plot_sensitivity_specificity <- function(x, display_cutpoint = TRUE, ...) {
    stopifnot("cutpointr" %in% class(x))
    args <- list(...)

    if (is.null(suppressWarnings(x$subgroup))) {
        dts_pr <- c("roc_curve", "optimal_cutpoint")
        ltype <- NULL
    } else {
        dts_pr <- c("roc_curve", "subgroup", "optimal_cutpoint")
        ltype <- ~ subgroup
    }

    if (suppressWarnings(is.null(x$subgroup))) {
        plot_title <- ggplot2::ggtitle("Sensitivity and specificity plot")
    } else {
        plot_title <- ggplot2::ggtitle("Sensitivity and specificity plot",
                                       "by class")
    }
    for (r in 1:nrow(x)) {
        x$roc_curve[[r]] <- x$roc_curve[[r]] %>%
            dplyr::mutate_(Sensitivity = ~ tp / (tp + fn),
                           Specificity = ~ tn / (tn + fp))
    }
    res_unnested <- x %>%
        dplyr::select_(.dots = dts_pr) %>%
        tidyr::unnest_(unnest_cols = c("roc_curve"))
    res_unnested <- x %>%
        tidyr::unnest_(unnest_cols = c("roc_curve"))
    # Drop possible NaN at x.sorted = Inf or -Inf
    res_unnested <- stats::na.omit(res_unnested)
    res_unnested <- tidyr::gather_(res_unnested, key_col = "metric",
                                    value_col = "value",
                                    gather_cols = c("Sensitivity", "Specificity"))
    pr <- ggplot2::ggplot(res_unnested,
                          ggplot2::aes_(x = ~ x.sorted, y = ~ value,
                                        color = ~ metric, linetype = ltype)) +
        ggplot2::geom_line() +
        plot_title +
        ggplot2::xlab("Cutpoint") +
        ggplot2::ylab("Sensitivity and Specificity")
    if (display_cutpoint) {
        if (is.null(suppressWarnings(x$subgroup))) {
            res_cutpoints <- x %>%
                dplyr::select_(.dots = "optimal_cutpoint")
            if (is.list(res_cutpoints$optimal_cutpoint)) {
                res_cutpoints <- tidyr::unnest_(res_cutpoints)
            }
        } else {
            res_cutpoints <- x %>%
                dplyr::select_(.dots = list("optimal_cutpoint", "subgroup"))
            if (is.list(res_cutpoints$optimal_cutpoint)) {
                res_cutpoints <- tidyr::unnest_(res_cutpoints)
            }
        }
        pr <- pr +
            ggplot2::geom_vline(data = res_cutpoints,
                                ggplot2::aes_(xintercept = ~ optimal_cutpoint,
                                              linetype = ltype))
    }
    return(pr)
}
