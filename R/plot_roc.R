#' Plot ROC curve from a cutpointr or roc_cutpointr object
#'
#' Given a \code{cutpointr} object this function plots the ROC curve(s)
#' per subgroup, if given. Also plots a ROC curve from the output of \code{roc()}.
#' @param x A cutpointr or roc_cutpointr object.
#' @param display_cutpoint (logical) Whether or not to display the optimal
#' cutpoint as a dot on the ROC curve for cutpointr objects.
#' @param type "line" for line plot (default) or "step" for step plot.
#' @param ... Additional arguments (unused).
#' @examples
#' opt_cut <- cutpointr(suicide, dsi, suicide)
#' plot_roc(opt_cut, display_cutpoint = FALSE)
#'
#' opt_cut_2groups <- cutpointr(suicide, dsi, suicide, gender)
#' plot_roc(opt_cut_2groups, display_cutpoint = TRUE)
#'
#' roc_curve <- roc(suicide, x = dsi, class = suicide, pos_class = "yes",
#'   neg_class = "no", direction = ">=")
#' plot(roc_curve)
#' auc(roc_curve)
#' @family cutpointr plotting functions
#' @name plot_roc
#' @export
plot_roc <- function(x, ...) {
    UseMethod("plot_roc", x)
}

#' @rdname plot_roc
#' @export
plot_roc.cutpointr <- function(x, display_cutpoint = TRUE, type = "line", ...) {
    predictor <- as.name(x$predictor[1])
    outcome <- as.name(x$outcome[1])

    if (!(has_column(x, "subgroup"))) {
        dts_roc <- "roc_curve"
        transparency <- 1
    } else {
        dts_roc <- c("roc_curve", "subgroup")
        transparency <- 0.6
    }

    if (!(has_column(x, "subgroup"))) {
        roc_title <- ggplot2::ggtitle("ROC curve")
    } else {
        roc_title <- ggplot2::ggtitle("ROC curve", "by class")
    }
    if (display_cutpoint) {
        optcut_coords <- purrr::pmap_df(x, function(...) {
            args <- list(...)
            opt_ind <- get_opt_ind(roc_curve = args$roc_curve,
                                   oc = args$optimal_cutpoint,
                                   direction = args$direction)
            data.frame(tpr = args$roc_curve$tpr[opt_ind],
                       tnr = args$roc_curve$tnr[opt_ind])

        })
    }
    res_unnested <- x %>%
        dplyr::select(dts_roc) %>%
        tidyr::unnest(.data$roc_curve)
    if (!(has_column(x, "subgroup"))) {
        roc <- ggplot2::ggplot(res_unnested,
                               ggplot2::aes(x = 1 - tnr, y = tpr)) +
            roc_title +
            ggplot2::xlab("1 - Specificity") +
            ggplot2::ylab("Sensitivity") +
            ggplot2::theme(aspect.ratio = 1)
    } else if (has_column(x, "subgroup")) {
        roc <- ggplot2::ggplot(res_unnested,
                               ggplot2::aes(x = 1 - tnr, y = tpr, color = subgroup)) +
            roc_title +
            ggplot2::xlab("1 - Specificity") +
            ggplot2::ylab("Sensitivity") +
            ggplot2::theme(aspect.ratio = 1)

    }
    if (display_cutpoint) {
        roc <- roc + ggplot2::geom_point(data = optcut_coords, color = "black")
    }
    if (type == "line") {
        roc <- roc + ggplot2::geom_line()
    } else if (type == "step") {
        roc <- roc + ggplot2::geom_step()
    }
    return(roc)
}

#' @rdname plot_roc
#' @export
plot_roc.roc_cutpointr <- function(x, type = "line", ...) {
    roc_title <- ggplot2::ggtitle("ROC curve")
    roc <- ggplot2::ggplot(x, ggplot2::aes(x = 1 - tnr, y = tpr)) +
        roc_title +
        ggplot2::xlab("1 - Specificity") +
        ggplot2::ylab("Sensitivity") +
        ggplot2::theme(aspect.ratio = 1)
    if (type == "line") {
        roc <- roc + ggplot2::geom_line()
    } else if (type == "step") {
        roc <- roc + ggplot2::geom_step()
    }
    return(roc)
}

#' @inherit plot_roc
#' @export
plot.roc_cutpointr <- function(x, type = "line", ...) {
    plot_roc(x, type = type)
}