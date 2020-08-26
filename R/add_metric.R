#' Add metrics to a cutpointr or roc_cutpointr object
#'
#' By default, the output of cutpointr includes the optimized metric and several
#' other metrics. This function adds further metrics. Suitable metric functions
#' are all metric functions that are included in the package or that comply
#' with those standards.
#'
#' @param object A cutpointr or roc_cutpointr object.
#' @param metric (list) A list of metric functions to be added.
#' @return A cutpointr or roc_cutpointr object (a data.frame) with one or more added columns.
#' @examples
#' library(dplyr)
#' library(cutpointr)
#' cutpointr(suicide, dsi, suicide, gender) %>%
#'   add_metric(list(ppv, npv)) %>%
#'   select(optimal_cutpoint, subgroup, AUC, sum_sens_spec, ppv, npv)
#' @export
#' @family main cutpointr functions
#' @importFrom rlang !! :=
add_metric <- function(object, metric) {
    if (!is.list(metric)) stop("The metric function(s) must be given as a list.")
    if (!(("cutpointr" %in% class(object)) | ("roc_cutpointr" %in% class(object)))) {
        stop("add_metric only supports cutpointr and roc_cutpointr objects.")
    }
    if ("cutpointr" %in% class(object)) {
        met <- purrr::map(metric, function(metric_func) {
            if (!is.function(metric_func)) {
                stop("The list elements of metric have to be functions.")
            }
            # Get numbers of TP, FP, TN, FN at optimal cutpoint(s) from ROC curve(s)
            # and calculate the metric(s). Allow for type instability of the metric func:
            one_met <- purrr::pmap_df(list(object$optimal_cutpoint,
                                           object$roc_curve,
                                           object$direction),
                                      function(optimal_cutpoint, roc_curve, direction) {
                                          opt_ind <- get_opt_ind(roc_curve = roc_curve,
                                                                             oc = optimal_cutpoint,
                                                                             direction = direction)
                                          met <- metric_func(tp = roc_curve$tp[opt_ind],
                                                             fp = roc_curve$fp[opt_ind],
                                                             tn = roc_curve$tn[opt_ind],
                                                             fn = roc_curve$fn[opt_ind])
                                          met_name <- colnames(met)
                                          if (length(met_name) > 1) {
                                              stop("The metric function should return one column or a vector.")
                                          }
                                          if (is.null(met_name)) met_name <- "added_metric"
                                          if (length(met) > 1) {
                                              met <- list(as.numeric(unlist(met)))
                                          } else {
                                              met <- as.numeric(met)
                                          }
                                          tibble::tibble(!!met_name := met)
                                      })
            class(one_met) <- class(object)
            return(one_met)
        })
        oc <- dplyr::bind_cols(object, met)
        if (has_boot_results(oc)) {
            boot_augmented <- purrr::map(oc$boot, function(boot_group) {
                add_per_subgroup <- purrr::map(metric, function(metric_func) {
                    metric_robust <- function(metric_func, tp, fp, tn, fn) {
                        tp <- unlist(tp)
                        fp <- unlist(fp)
                        tn <- unlist(tn)
                        fn <- unlist(fn)
                        met <- as.numeric(metric_func(tp = tp,
                                                      fp = fp,
                                                      tn = tn,
                                                      fn = fn))
                        if (length(met) > 1) met <- list(met)
                        return(met)
                    }
                    one_met <- purrr::map(1:nrow(boot_group), function(r) {
                        met_b <- metric_robust(metric_func = metric_func,
                                               tp = boot_group$TP_b[r],
                                               fp = boot_group$FP_b[r],
                                               tn = boot_group$TN_b[r],
                                               fn = boot_group$FN_b[r])
                        met_oob <- metric_robust(metric_func = metric_func,
                                                 tp = boot_group$TP_oob[r],
                                                 fp = boot_group$FP_oob[r],
                                                 tn = boot_group$TN_oob[r],
                                                 fn = boot_group$FN_oob[r])
                        tibble::tibble(met_b, met_oob)
                    })
                    # Bind rows allowing for multiple optimal cutpoints
                    has_multiple <- any(
                        purrr::map_lgl(one_met, function(x) {
                            length(unlist(x)) > 2
                        })
                    )
                    if (has_multiple) {
                        one_met <- purrr::map_df(one_met, function(x) {
                            if (length(unlist(x)) == 2) {
                                tibble::tibble(met_b = list(x$met_b),
                                               met_oob = list(x$met_oob))
                            } else {
                                tibble::tibble(met_b = x$met_b,
                                               met_oob = x$met_oob)
                            }
                        })
                    } else {
                        one_met <- dplyr::bind_rows(one_met)
                    }
                    met_name <- colnames(metric_func(1,1,1,1)) # just get the name...
                    if (length(met_name) > 1) {
                        stop("The metric function should return one column or a vector.")
                    }
                    if (is.null(met_name)) met_name <- "added_metric"
                    colnames(one_met) <- paste0(met_name, c("_b", "_oob"))
                    return(one_met)
                })
                add_per_subgroup <- dplyr::bind_cols(add_per_subgroup)
                boot_group <- dplyr::bind_cols(boot_group, add_per_subgroup)
                return(boot_group)
            })
            oc$boot <- boot_augmented
        }
        return(oc)
    } else if ("roc_cutpointr" %in% class(object)) {
        met <- purrr::map(metric, function(metric_func) {
            if (!is.function(metric_func)) {
                stop("The list elements of metric have to be functions.")
            }
            # Get numbers of TP, FP, TN, FN at optimal cutpoint(s) from ROC curve(s)
            # and calculate the metric(s). Allow for type instability of the metric func:
            met <- metric_func(tp = object$tp,
                               fp = object$fp,
                               tn = object$tn,
                               fn = object$fn)
            met_name <- colnames(met)
            if (length(met_name) > 1) {
                stop("The metric function should return one column or a vector.")
            }
            if (is.null(met_name)) met_name <- "added_metric"
            met <- as.numeric(met)
            met <- tibble::tibble(!!met_name := met)
            class(met) <- class(object)
            return(met)
        })
        roc_added <- dplyr::bind_cols(object, met)
        return(roc_added)
    }
}
