#' Set a manual cutpoint for use with cutpointr
#'
#' Mainly useful if bootstrap estimates of the out-of-bag performance of a
#' given cutpoint are desired, e.g. taking a cutpoint value from the literature.
#'
#' @export
oc_manual <- function(cutpoint, ...) {
    return(data.frame(optimal_cutpoint = cutpoint))
}
