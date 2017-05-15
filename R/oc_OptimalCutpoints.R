#' A tidy wrapper for optimal.cutpoints
#'
#' Determine "optimal" cutpoints using optimal.cutpoints and its included methods
#' from the OptimalCutpoints package. The output will be a tidy data frame.of
#' the optimal cutpoint and the value of the metric, depending on methods, that
#' was obtained in-sample.
#'
#' @inheritParams oc_youden_normal
#' @param oc_metric (character) See ?optimal.cutpoints for available methods.
#' @param break_ties (function) A function to handle multiple optimal cutpoints,
#' if multiple optimal cutpoints are found.
#' @return A data frame with one row, the column optimal_cutpoint and a second
#' column named after method that gives the obtained metric value.
#' @examples
#' data(suicide)
#' if (require(OptimalCutpoints)) {
#'   oc_OptimalCutpoints(suicide, "dsi", "suicide", oc_metric = "Youden",
#'   neg_class = "no", direction = ">=")
#' }
#' @export
oc_OptimalCutpoints <- function(data, x, class, oc_metric,
                                neg_class, direction, break_ties = mean, ...) {
    cl <- match.call()
    if(is.null(suppressWarnings(cl$oc_metric)) | missing(oc_metric)) {
        stop("'oc_metric' argument required for oc_OptimalCutpoints")
    }
    metric_name <- oc_metric

    # Reverse direction because optimal.cutpoints "thinks" in terms of tag.healthy
    if (direction == ">=" | direction == ">") {
        direction <- "<"
    } else if (direction == "<=" | direction == "<") {
        direction <- ">"
    }
    mod <- OptimalCutpoints::optimal.cutpoints.default(X = x,
                                     status = class,
                                     data = as.data.frame(data), # dislikes tibbles
                                     methods = oc_metric, tag.healthy = neg_class,
                                     direction = direction)
    mod        <- mod[[metric_name]]
    opt_metric <- mod$Global$optimal.criterion

    oc  <- break_ties(mod$Global$optimal.cutoff$cutoff)
    res <- data.frame(optimal_cutpoint = oc,
                      metric           = opt_metric)
    colnames(res) <- c("optimal_cutpoint", metric_name)
    return(res)
}


