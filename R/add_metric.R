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
    stopifnot(("cutpointr" %in% class(object)) | ("roc_cutpointr" %in% class(object)))
    if ("cutpointr" %in% class(object)) {
        met <- purrr::map(metric, function(metric_func) {
            if (!is.function(metric_func)) {
                stop("The list elements of metric have to be functions.")
            }
            # Get numbers of TP, FP, TN, FN at optimal cutpoint(s) from ROC curve(s)
            # and calculate the metric(s). Allow for type instability of the metric func:
            purrr::pmap_df(list(object$optimal_cutpoint,
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
        })
        oc <- dplyr::bind_cols(object, met)
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
            tibble::tibble(!!met_name := met)
        })
        roc_added <- dplyr::bind_cols(object, met)
        return(roc_added)
    }
}