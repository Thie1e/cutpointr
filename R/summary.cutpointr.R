#' @export
summary.cutpointr <- function(object, ...) {
    class(object) <- c("summary_cutpointr", class(object))
    return(object)
}
