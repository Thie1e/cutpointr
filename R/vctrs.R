#' @importFrom vctrs vec_ptype2 vec_cast
NULL

# roc ---------------------------------------------------------------------

as_roc_cutpointr <- function(x) {
  stopifnot(is.data.frame(x))
  class(x) <- c("roc_cutpointr", "tbl_df", "tbl", "data.frame")
  x
}

roc_ptype <- function(x, y, ...) {
  as_roc_cutpointr(vctrs::df_ptype2(x, y, ...))
}
roc_cast <- function(x, y, ...) {
  as_roc_cutpointr(vctrs::df_cast(x, y, ...))
}

#' @export
vec_ptype2.roc_cutpointr.roc_cutpointr <- function(x, y, ...) {
  roc_ptype(x, y, ...)
}
#' @export
vec_ptype2.roc_cutpointr.tbl_df <- function(x, y, ...) {
  roc_ptype(x, y, ...)
}
#' @export
vec_ptype2.tbl_df.roc_cutpointr <- function(x, y, ...) {
  roc_ptype(x, y, ...)
}
#' @export
vec_ptype2.roc_cutpointr.data.frame <- function(x, y, ...) {
  roc_ptype(x, y, ...)
}
#' @export
vec_ptype2.data.frame.roc_cutpointr <- function(x, y, ...) {
  roc_ptype(x, y, ...)
}

#' @export
vec_cast.roc_cutpointr.roc_cutpointr <- function(x, to, ...) {
  roc_cast(x, to, ...)
}
#' @export
vec_cast.roc_cutpointr.tbl_df <- function(x, to, ...) {
  roc_cast(x, to, ...)
}
#' @export
vec_cast.tbl_df.roc_cutpointr <- function(x, to, ...) {
  roc_cast(x, to, ...)
}
#' @export
vec_cast.roc_cutpointr.data.frame <- function(x, to, ...) {
  roc_cast(x, to, ...)
}
#' @export
vec_cast.data.frame.roc_cutpointr <- function(x, to, ...) {
  roc_cast(x, to, ...)
}


# cutpointr ---------------------------------------------------------------

as_cutpointr <- function(x) {
  stopifnot(is.data.frame(x))
  class(x) <- c("cutpointr", "tbl_df", "tbl", "data.frame")
  x
}

cutpointr_ptype <- function(x, y, ...) {
  as_cutpointr(vctrs::df_ptype2(x, y, ...))
}
cutpointr_cast <- function(x, y, ...) {
  as_cutpointr(vctrs::df_cast(x, y, ...))
}

#' @export
vec_ptype2.cutpointr.cutpointr <- function(x, y, ...) {
  cutpointr_ptype(x, y, ...)
}
#' @export
vec_ptype2.cutpointr.tbl_df <- function(x, y, ...) {
  cutpointr_ptype(x, y, ...)
}
#' @export
vec_ptype2.tbl_df.cutpointr <- function(x, y, ...) {
  cutpointr_ptype(x, y, ...)
}
#' @export
vec_ptype2.cutpointr.data.frame <- function(x, y, ...) {
  cutpointr_ptype(x, y, ...)
}
#' @export
vec_ptype2.data.frame.cutpointr <- function(x, y, ...) {
  cutpointr_ptype(x, y, ...)
}

#' @export
vec_cast.cutpointr.cutpointr <- function(x, to, ...) {
  cutpointr_cast(x, to, ...)
}
#' @export
vec_cast.cutpointr.tbl_df <- function(x, to, ...) {
  cutpointr_cast(x, to, ...)
}
#' @export
vec_cast.tbl_df.cutpointr <- function(x, to, ...) {
  cutpointr_cast(x, to, ...)
}
#' @export
vec_cast.cutpointr.data.frame <- function(x, to, ...) {
  cutpointr_cast(x, to, ...)
}
#' @export
vec_cast.data.frame.cutpointr <- function(x, to, ...) {
  cutpointr_cast(x, to, ...)
}
