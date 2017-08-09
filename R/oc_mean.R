#' Use the sample mean as cutpoint
#'
#' The sample mean is calculated and returned as the "optimal" cutpoint.
#'
#' @param data A data frame or tibble in which the columns that are given in x
#' and class can be found.
#' @param x (character) The variable name to be used for classification,
#' e.g. predictions or test values.
#' @param trim The fraction (0 to 0.5) of observations to be trimmed from each
#' end of x before the mean is computed. Values of trim outside that range are
#' taken as the nearest endpoint.
#' @param ... To capture further arguments that are always passed to the method
#' function by cutpointr. The cutpointr function passes data, x, class,
#' metric_func, direction, pos_class and neg_class to the method function.
#' @examples
#' data(suicide)
#' oc_mean(suicide, "dsi")
#' @export
oc_mean <- function(data, x, trim = 0, ...) {
    stopifnot(is.character(x))
    return(data.frame(optimal_cutpoint = mean(unlist(data[, x]), trim = trim)))
}
