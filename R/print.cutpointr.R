#' @export
print.cutpointr <- function(x, ...) {
    tibble:::print.tbl_df(x, width = Inf)
}