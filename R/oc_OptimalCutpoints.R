#' @export
oc_OptimalCutpoints <- function(data, x, class, methods,
                                pos_class, neg_class, direction, ...) {
    if (any(direction == c(">=", "<="))) stop("OptimalCutpoints only supports < and >")
    # stopifnot(is.character(x))
    # stopifnot(is.character(class))
    cl <- match.call()
    metric_name <- cl$methods
    neg_class <- unique(unlist(data[, class]))
    neg_class <- neg_class[neg_class != pos_class]
    # Reverse direction because optimal.cutpoints thinks in terms of tag.healthy
    if (direction == ">") {
        direction <- "<"
    } else if (direction == "<") {
        direction <- ">"
    }
    mod <- optimal.cutpoints.default(X = x,
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


# oc_OptimalCutpoints(elas$elas, elas$status, methods = "Youden", pos_class = 1, higher = T)

# cutpointr(elas, elas, status, pos_class = 1,
#           optcut_func = oc_OptimalCutpoints, methods = "Youden")



