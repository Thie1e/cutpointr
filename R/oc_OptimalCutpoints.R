#' @export
oc_OptimalCutpoints <- function(x, class, methods,
                                pos_class = pos_class, higher = higher, ...) {
    # if (any(direction == c(">=", "<="))) stop("OptimalCutpoints only supports < and >")
    cl <- match.call()
    metric_name <- cl$methods
    dat <- data.frame(x, class)
    neg_class <- unique(class)
    neg_class <- neg_class[neg_class != pos_class]
    # Reverse direction because optimal.cutpoints thinks in terms of tag.healthy
    # if (direction == ">") direction <- "<"
    if (higher) direction <- "<" else direction <- ">"
    mod <- optimal.cutpoints.default(X = colnames(dat)[1],
                                     status = colnames(dat)[2],
                                     data = dat,
                                     methods = methods, tag.healthy = neg_class,
                                     direction = direction)
    mod     <- mod[[metric_name]]
    oc_metric <- mod$Global$optimal.criterion
    oc     <- mod$Global$optimal.cutoff$cutoff
    res <- data.frame(optimal_cutpoint = oc,
                      metric           = oc_metric)
    colnames(res) <- c("optimal_cutpoint", metric_name)
    return(res)
}


# oc_OptimalCutpoints(elas$elas, elas$status, methods = "Youden", pos_class = 1, higher = T)

# cutpointr(elas, elas, status, pos_class = 1,
#           optcut_func = oc_OptimalCutpoints, methods = "Youden")



