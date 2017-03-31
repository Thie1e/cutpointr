#' @source tibble
#' @export
print.cutpointr <- function (x, ..., n = NULL, width = Inf, n_extra = NULL) {
    print(tibble::trunc_mat(x, n = n, width = width, n_extra = n_extra))
    invisible(x)
}
