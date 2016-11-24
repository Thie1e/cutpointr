#' Evaluate performance at all possible cutpoints using various methods and metrics
#'
#' Cutpoints are taken from the sample values (with the optional addition of midpoints
#' between these values) and the classification quality of these cupoints is
#' evaluated based on the chosen method.
#' Some methods, like "kern_smooth", use smoothed densities instead of the original
#' sample values. In that case the returned metrics refer to the smoothed data
#' instead of the original values.
#'
#' @param x (numeric vector) The variable to be used for classification, e.g. test values.
#' @param class (factor) The class of each observation, e.g. control or treatment group.
#' @param group (factor) An additional covariate that identifies subgroups. Separate
#' @param method (character) The method for determining the optimal cutpoint and the
#' metric values at all possible cutpoints. See details for all included methods.
#' cutpoints and metrics will be calculated for every group.
#' @export
eval_cutpoints <- function(x, class, group, method, ...){
    UseMethod("eval_cutpoints")
}

#' @rdname eval_cutpoints
#' @param foo (numeric) random number please
#' @export
eval_cutpoints.default = function(x, ...) {
    # do some magic
}

#' @rdname eval_cutpoints
#' @param formula (formula) Formula for splitting classes by some numeric vector, e.g. class ~ x.
#' @export
eval_cutpoints.formula = function(x, ...) {
    # do some magic
}