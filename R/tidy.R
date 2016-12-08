#' Extract the optimal cutpoint from a cutpointr object or a cp_result object or a data.frame
#'
#' In the spirit of broom::tidy. Works on any data.frame that has two columns.
#' The expected column name for the cutpoints is "cutpoint". The other one will
#' be treated as the metric to be maximized (or minimized). If no column named
#' "cutpoint" is found the first column will be treated as the column of cutpoints.
#'
#' @param object (cutpointr) A data frame with the column "cutpoint" as the first
#' column and a second column of metric values.
#' @param maximize (logical) Whether to maximize or minimize the metric.
#' Default TRUE.
#' @param break_ties (logical) If TRUE (default) the function will
#' return the median cutoff of all cutoff values that maximize the metric.
#' Otherwise the first (lowest) cutoff that maximizes the metric will be returned.

# Ich glaube das würde broom::tidy kaputt machen
# tidy <- function(object, maximize = T) {
#     UseMethod("tidy")
# }

#' @export
tidy.cutpointr <- function(data, maximize = T) {
    stop("Not yet implemented")
    data <- cutpointr$all_cutpoints # oder so
    cutpointr::tidy.cp_result(data)
}

#' @export
tidy.cp_result <- function(cp_result) {
    warning("How to break ties here? Metric possibly optimistic.")
    max_m <- max(cp_result[, 2])
    max_ind <- which(cp_result[, 2] == max_m)
    opt_cut <- median(cp_result[max_ind, "cutpoint"])
    opt_metric <- max_m
}
