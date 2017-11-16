#' Print cutpointr objects
#'
#' Prints the cutpointr object with full width like a tbl_df.
#'
#' @source Kirill Müller and Hadley Wickham (2017). tibble: Simple Data Frames.
#'  https://CRAN.R-project.org/package=tibble
#' @param x a cutpointr object.
#' @param width width of output.
#' @param n number of rows to print.
#' @param ... further arguments.
#' @export
print.cutpointr <- function(x, width = 1000, n = 50, ...) {
    if (utils::packageVersion("tibble") <= "1.3.4") {
        x %>%
            utils::head(n = n) %>%
            as.data.frame() %>%
            tibble:::shrink_mat(width = width, rows = NA, n = n, star = FALSE) %>%
            `[[`("table") %>%
            print()
        invisible(x)
    } else {
        tibble:::print.tbl_df(x, width = width, n = n)
        invisible(x)
    }
}
