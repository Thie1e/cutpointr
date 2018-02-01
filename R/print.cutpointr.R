#' Print cutpointr objects
#'
#' Prints the \code{cutpointr} object with full width like a \code{tbl_df}.
#'
#' @source Kirill Müller and Hadley Wickham (2017). tibble: Simple Data Frames.
#'  https://CRAN.R-project.org/package=tibble
#' @param x a cutpointr object.
#' @param width width of output.
#' @param n number of rows to print.
#' @param ... further arguments.
#' @export
print.cutpointr <- function(x, width = 1000, n = 50, ...) {
    # print.tbl_df is not exported by tibble, avoid :::
    class(x) <- c("tbl_df", "tbl", "data.frame")
    print(x, width = width, n = n)
    class(x) <- c("cutpointr", "tbl_df", "tbl", "data.frame")
    invisible(x)
}
