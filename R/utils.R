#' Calculate AUC
#' @source MESS::auc
auc <- function (x, y, from = min(x), to = max(x))
{
    # MESS::auc with type = "linear" and absolutearea = F
    if (length(x) != length(y))
        stop("x and y must have the same length")
    if (length(unique(x)) < 2)
        return(NA)
    values <- approx(x, y, xout = sort(unique(c(from, to, x[x > from & x < to]))), ...)
    0.5 * sum(diff(values$x) * (values$y[-1] + values$y[-length(values$y)]))
}