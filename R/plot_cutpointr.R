#' General purpose plotting function for cutpointr or roc_cutpointr objects
#'
#' Flexibly plot various metrics against all cutpoints or any other metric.
#' The function can plot any metric based on a \code{cutpointr} or \code{roc_cutpointr}
#' object. If \code{cutpointr} was run with bootstrapping, bootstrapped confidence
#' intervals can be plotted. These represent the quantiles of the distribution
#' of the y-variable grouped by x-variable over all bootstrap repetitions.
#'
#' The arguments to \code{xvar} and \code{yvar} should be metric functions. Any metric
#' function that is suitable for \code{cutpointr} can also be used in \code{plot_cutpointr}.
#' Anonymous functions are also allowed.
#' To plot all possible cutpoints, the utility function \code{cutpoint} can be used.
#'
#' The functions for \code{xvar} and \code{yvar} may accept any or all of the arguments
#' \code{tp}, \code{fp}, \code{tn}, or \code{fn} and return a numeric vector,
#' a matrix or a \code{data.frame}.
#' For more details on metric functions see \code{vignette("cutpointr")}.
#'
#' Note that confidence intervals can only be correctly plotted if the values of \code{xvar}
#' are constant across bootstrap samples. For example, confidence intervals for
#' \code{tpr} by \code{fpr} (a ROC curve) cannot be plotted, as the values of the false positive
#' rate vary per bootstrap sample.
#'
#' @param x A \code{cutpointr} or \code{roc_cutpointr} object.
#' @param xvar A function, typically \code{cutpoint} or a metric function.
#' @param yvar A function, typically a metric function.
#' @param conf_lvl (numeric) If bootstrapping was run and x is a cutpointr object,
#' a confidence interval at the level of conf_lvl can be plotted. To plot no
#' confidence interval set conf_lvl = 0.
#' @param aspect_ratio (numeric) Set to 1 to obtain a quadratic plot, e.g. for
#' plotting a ROC curve.
#'
#' @examples
#' set.seed(1)
#' oc <- cutpointr(suicide, dsi, suicide, boot_runs = 10)
#'
#' plot_cutpointr(oc, cutpoint, F1_score)
#'
#' ## ROC curve
#' plot_cutpointr(oc, fpr, tpr, aspect_ratio = 1)
#'
#' ## Custom function
#' plot_cutpointr(oc, cutpoint, function(tp, tn, fp, fn, ...) tp / fp) +
#'   ggplot2::ggtitle("Custom metric") + ggplot2::ylab("value")
#'
#' @family cutpointr plotting functions
#' @export
plot_cutpointr <- function(x, xvar = cutpoint, yvar = sum_sens_spec,
                           conf_lvl = 0.95, aspect_ratio = NULL) {
    stopifnot("cutpointr" %in% class(x) | "roc_cutpointr" %in% class(x))
    if ("cutpointr" %in% class(x)) {
        rocdat <- x$roc_curve
        subgroup <- suppressWarnings(x$subgroup)
    } else {
        rocdat <- list(x)
        subgroup <- NULL
    }

    xvar_name <- paste(as.character(substitute(xvar)), collapse = " ")
    yvar_name <- paste(as.character(substitute(yvar)), collapse = " ")
    xvar_name_plotlabel <- xvar_name
    yvar_name_plotlabel <- yvar_name
    rocdat <- purrr::map(.x = rocdat, .f = function(x) {
        met <- xvar(x = x, tp = x$tp, fp = x$fp, tn = x$tn, fn = x$fn)
        met_name <- colnames(met)
        if (is.null(met_name)) {
            met_name <- xvar_name
        } else {
            xvar_name <<- met_name
            xvar_name_plotlabel <<- met_name
        }
        met <- sanitize_metric(met, m_name = met_name, n = nrow(x))
        met <- check_metric_name(met)
        xvar_name <<- colnames(met)
        met <- tibble::as_tibble(met)
        class(met) <- class(x) # Avoid warning from vctrs
        x <- dplyr::bind_cols(x, met)
        x
    })
    rocdat <- purrr::map(.x = rocdat, .f = function(x) {
        met <- yvar(x = x, tp = x$tp, fp = x$fp, tn = x$tn, fn = x$fn)
        met_name <- colnames(met)

        if (is.null(met_name)) {
            met_name <- xvar_name
        } else {
            yvar_name <<- met_name
            yvar_name_plotlabel <<- met_name
        }
        met <- sanitize_metric(met, m_name = yvar_name, n = nrow(x))
        met <- check_metric_name(met)
        yvar_name <<- colnames(met)
        met <- tibble::as_tibble(met)
        class(met) <- class(x) # Avoid warning from vctrs
        x <- dplyr::bind_cols(x, met)
        x
    })

    if (has_boot_results(x) & conf_lvl != 0) {
        # Add xvar and yvar columns to ROC curves of bootstrap repetitions
        for (i in 1:nrow(x)) {
            x[["boot"]][[i]]$roc_curve_b <- purrr::map(x[["boot"]][[i]]$roc_curve_b, function(x) {
                met <- xvar(x = x, tp = x$tp, fp = x$fp, tn = x$tn, fn = x$fn)
                met <- sanitize_metric(met, m_name = xvar_name, n = nrow(x),
                                                   silent = TRUE)
                met <- check_metric_name(met)
                met <- tibble::as_tibble(met)
                class(met) <- class(x) # Avoid warning from vctrs
                x <- dplyr::bind_cols(x, met)
                met <- yvar(x = x, tp = x$tp, fp = x$fp, tn = x$tn, fn = x$fn)
                met <- sanitize_metric(met, m_name = yvar_name, n = nrow(x),
                                                   silent = TRUE)
                met <- check_metric_name(met)
                met <- tibble::as_tibble(met)
                class(met) <- class(x) # Avoid warning from vctrs
                x <- dplyr::bind_cols(x, met)
                x
            })
            ci <- x[["boot"]][[i]]$roc_curve_b %>%
                dplyr::bind_rows() %>%
                dplyr::select(!!rlang::sym(xvar_name), !!rlang::sym(yvar_name)) %>%
                dplyr::group_by(!!rlang::sym(xvar_name)) %>%
                dplyr::summarise(ymin = stats::quantile(!!rlang::sym(yvar_name),
                                                        (1 - conf_lvl) / 2, na.rm = TRUE),
                                 ymax = stats::quantile(!!rlang::sym(yvar_name),
                                                        1 - (1 - conf_lvl) / 2, na.rm = TRUE))
            rocdat[[i]] <- dplyr::left_join(rocdat[[i]], ci, by = xvar_name)
            if (any(!(rocdat[[i]][[xvar_name]] %in% ci[[xvar_name]])))
                warning(paste(x$subgroup[i],
                              "Not all x-values in ROC curve could be joined with bootstrap,",
                              "the bootstrap confidence intervals are possibly misleading,",
                              "see ?plot_cutpointr"))
        }
    }

    if ("subgroup" %in% colnames(x)) {
        rocdat <- purrr::map2(subgroup, rocdat, function(s, d) {
            d <- d[is.finite(d[[yvar_name]]), ]
            d <- d[is.finite(d[[xvar_name]]), ]
            d$subgroup <- s
            return(d)
        }) %>%
            dplyr::bind_rows()
        if (has_boot_results(x) & conf_lvl != 0) {
            p <- ggplot2::ggplot(rocdat, ggplot2::aes_string(x = xvar_name,
                                                             y = yvar_name,
                                                             ymax = "ymax",
                                                             ymin = "ymin",
                                                             fill = "subgroup",
                                                             color = "subgroup")) +
                ggplot2::geom_line() +
                ggplot2::geom_ribbon(alpha = 0.2, size = 0) +
                ggplot2::ggtitle(paste(yvar_name_plotlabel, "by", xvar_name_plotlabel),
                                 "in-sample results") +
                ggplot2::ylab(yvar_name_plotlabel) +
                ggplot2::xlab(xvar_name_plotlabel)
        } else {
            p <- ggplot2::ggplot(rocdat, ggplot2::aes_string(x = xvar_name,
                                                                   y = yvar_name,
                                                                   color = "subgroup")) +
                ggplot2::geom_line() +
                ggplot2::ggtitle(paste(yvar_name_plotlabel, "by", xvar_name_plotlabel),
                                 "in-sample results") +
                ggplot2::ylab(yvar_name_plotlabel) +
                ggplot2::xlab(xvar_name_plotlabel)
        }
    } else {
        rocdat <- rocdat[[1]]
        rocdat <- rocdat[is.finite(rocdat[[yvar_name]]), ]
        rocdat <- rocdat[is.finite(rocdat[[xvar_name]]), ]
        if (has_boot_results(x) & conf_lvl != 0) {
            p <- ggplot2::ggplot(rocdat, ggplot2::aes_string(x = xvar_name,
                                                       y = yvar_name,
                                                       ymax = "ymax",
                                                       ymin = "ymin")) +
                ggplot2::geom_line() +
                ggplot2::geom_ribbon(alpha = 0.2, size = 0) +
                ggplot2::ggtitle(paste(yvar_name_plotlabel, "by", xvar_name_plotlabel),
                                 "in-sample results") +
                ggplot2::ylab(yvar_name_plotlabel) +
                ggplot2::xlab(xvar_name_plotlabel)
        } else {
            p <- ggplot2::ggplot(rocdat, ggplot2::aes_string(x = xvar_name,
                                                       y = yvar_name)) +
                ggplot2::geom_line() +
                ggplot2::ggtitle(paste(yvar_name_plotlabel, "by", xvar_name_plotlabel),
                                 "in-sample results") +
                ggplot2::ylab(yvar_name_plotlabel) +
                ggplot2::xlab(xvar_name_plotlabel)
        }
    }
    if (!(is.null(aspect_ratio))) p <- p + ggplot2::theme(aspect.ratio = aspect_ratio)
    return(p)
}
