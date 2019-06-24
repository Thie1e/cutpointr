#' Plot ROC curve from a cutpointr object
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
#' @family cutpointr plotting functions
#' @export
plot_roc <- function(x, display_cutpoint = TRUE, type = "line", ...) {
    args <- list(...)
    if ("cutpointr" %in% class(x)) {
        plot_roc_cp(x = x, display_cutpoint = display_cutpoint, type = type, args)
    } else if ("roc_cutpointr" %in% class(x)) {
        plot_roc_rcp(x = x, type = type, args)
    } else {
        stop(paste("Can only plot ROC curve from objects of types cutpointr",
                   "and roc_cutpointr"))
    }
}

plot_roc_cp <- function(x, display_cutpoint, type, ...) {
    predictor <- as.name(x$predictor[1])
    outcome <- as.name(x$outcome[1])

    if (!(has_column(x, "subgroup"))) {
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

    if (!(has_column(x, "subgroup"))) {
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
        roc_title +
        ggplot2::xlab("1 - Specificity") +
        ggplot2::ylab("Sensitivity") +
        ggplot2::theme(aspect.ratio = 1)
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

plot_roc_rcp <- function(x, type, ...) {
    roc_title <- ggplot2::ggtitle("ROC curve")
    roc <- ggplot2::ggplot(x, ggplot2::aes_(x = ~ 1 - tnr, y = ~ tpr)) +
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
