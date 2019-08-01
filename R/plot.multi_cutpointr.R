#' Plotting multi_cutpointr objects is currently not supported
#'
#' You can try plotting the data manually instead.
#'
#' @param x A multi_cutpointr object.
#' @param ... Further arguments.
#'
#' @export
plot.multi_cutpointr <- function(x, ...) {
    stop("Plotting multi_cutpointr objects is not supported.")
    return(invisible(NULL))
}
