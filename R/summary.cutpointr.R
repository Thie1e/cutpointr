#' @export
summary.cutpointr <- function(cutpointr) {
    class(cutpointr) <- c("summary_cutpointr", class(cutpointr))
    return(cutpointr)
}
