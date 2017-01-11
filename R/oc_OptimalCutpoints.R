#' A tidy wrapper for optimal.cutpoints
#'
#' Determine "optimal" cutpoints using optimal.cutpoints and its included methods
#' from the OptimalCutpoints package. The output will be a tidy data frame.of
#' the optimal cutpoint and the value of the metric, depending on methods, that
#' was obtained in-sample.
#'
#' @inheritParams cutpointr
#' @param methods (character) See ?optimal.cutpoints for available methods.
#' @return A data frame with one row, the column optimal_cutpoint and a second
#' column named after method that gives the obtained metric value.
#' @examples
#' library(OptimalCutpoints)
#' data(elas)
#' oc_OptimalCutpoints(elas, "elas", "status", methods = "Youden", pos_class = 1,
#' direction = ">")
#' @export
oc_OptimalCutpoints <- function(data, x, class, methods,
                                pos_class, neg_class, direction, ...) {
    cl <- match.call()
    metric_name <- cl$methods
    neg_class <- unique(unlist(data[, class]))
    neg_class <- neg_class[neg_class != pos_class]
    # Reverse direction because optimal.cutpoints "thinks" in terms of tag.healthy
    if (direction == ">") {
        direction <- "<"
    } else if (direction == "<") {
        direction <- ">"
    }
    mod <- OptimalCutpoints::optimal.cutpoints.default(X = x,
                                     status = class,
                                     data = as.data.frame(data), # dislikes tibbles
                                     methods = methods, tag.healthy = neg_class,
                                     direction = direction)
    mod       <- mod[[metric_name]]
    oc_metric <- mod$Global$optimal.criterion
    oc        <- mean(mod$Global$optimal.cutoff$cutoff) # Break ties
    res <- data.frame(optimal_cutpoint = oc,
                      metric           = oc_metric)
    colnames(res) <- c("optimal_cutpoint", metric_name)
    return(res)
}




