#' Print cutpointr objects
#'
#' Prints the cutpointr object with infinite width like a tbl_df.
#'
#' @source Kirill Müller and Hadley Wickham (2017). tibble: Simple Data Frames.
#'  https://CRAN.R-project.org/package=tibble
#' @param x a cutpointr object.
#' @param n number of rows to print.
#' @param ... further arguements.
#' @export
print.cutpointr <- function(x, n = Inf, ...) {
    x %>%
        utils::head(n = n) %>%
        as.data.frame() %>%
        tibble:::shrink_mat(width = Inf, rows = NA, n = n, star = FALSE) %>%
        `[[`("table") %>%
        print()
    invisible(x)
}
