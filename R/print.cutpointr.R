#' @export
print.cutpointr <- function(cutpointr) {
    tibble:::print.tbl_df(cutpointr, width = Inf)
}