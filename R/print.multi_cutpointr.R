#' Print multi_cutpointr objects
#'
#' Prints the \code{multi_cutpointr} object with infinite width like a \code{tbl_df}.
#'
#' @source Kirill Müller and Hadley Wickham (2017). tibble: Simple Data Frames.
#'  https://CRAN.R-project.org/package=tibble
#' @param x a multi_cutpointr object.
#' @param n number of rows to print.
#' @param ... further arguments.
#' @export
print.multi_cutpointr <- function(x, n = Inf, ...) {
    print.cutpointr(x, n = n, ...)
}