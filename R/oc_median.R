#' Use the sample median as cutpoint
#'
#' The sample median is calculated and returned as the optimal cutpoint.
#'
#' @param data A data frame or tibble in which the columns that are given in x
#' and class can be found.
#' @param x (character) The variable name to be used for classification,
#' e.g. predictions or test values.
#' @param ... To capture further arguments that are always passed to the method
#' function by cutpointr. The cutpointr function passes data, x, class,
#' metric_func, direction, pos_class and neg_class to the method function.
#' @examples
#' data(suicide)
#' oc_median(suicide, "dsi")
#' cutpointr(suicide, dsi, suicide, method = oc_median)
#' @export
oc_median <- function(data, x, ...) {
    stopifnot(is.character(x))
    return(data.frame(optimal_cutpoint = stats::median(unlist(data[, x]))))
}