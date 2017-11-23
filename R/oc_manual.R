#' Set a manual cutpoint for use with cutpointr
#'
#' This function simply returns \code{cutpoint} as the optimal cutpoint.
#' Mainly useful if bootstrap estimates of the out-of-bag performance of a
#' given cutpoint are desired, e.g. taking a cutpoint value from the literature.
#'
#' @inheritParams oc_youden_normal
#' @param cutpoint (numeric) The fixed cutpoint.
#' @examples
#' cutpointr(suicide, dsi, suicide, method = oc_manual, cutpoint = 4)
#' @export
oc_manual <- function(cutpoint, ...) {
    return(data.frame(optimal_cutpoint = cutpoint))
}
