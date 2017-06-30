#' Print cutpointr objects
#'
#' Prints the cutpointr object like a tbl_df.
#' With versions of tibble later than 1.3.0 printing tibbles with `width = Inf`
#' does not wrap columns nicely so the default has been changed to `width = NULL`.
#'
#' @source Kirill Müller and Hadley Wickham (2017). tibble: Simple Data Frames.
#'  https://CRAN.R-project.org/package=tibble
#' @param x a cutpointr object.
#' @param n number of rows to print.
#' @param width width of text output to generate. This defaults to NULL,
#' which displays only the columns that fit on one screen. You can also set
#' width = Inf to override this default and always print all columns.
#' @param n_extra number of extra columns to print abbreviated information for,
#' if the width is too small for the entire tibble.
#' @param ... further arguements.
#' @export
print.cutpointr <- function (x, n = NULL, width = NULL, n_extra = NULL, ...) {
    print(tibble::trunc_mat(x, n = n, width = width, n_extra = n_extra))
    invisible(x)
}
