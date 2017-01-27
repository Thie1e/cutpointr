optimize_metric <- function(data, x, class, metric_func = youden,
                            pos_class = NULL, neg_class = NULL, minmax,
                            direction, metric_name = "metric") {
    metric_name_call <- as.character(substitute(metric_func))
    if (metric_name_call != "metric_func") metric_name <- metric_name_call
    roccurve <- roc(data = data, x = x, class = class, pos_class = pos_class,
                    neg_class = neg_class, direction = direction)
    m <- metric_func(tp = roccurve[, "tp"], fp = roccurve[, "fp"],
                     tn = roccurve[, "tn"], fn = roccurve[, "fn"])
    if (minmax == "max") {
        opt <- m == max(m)
        oc <- min(roccurve[, "x.sorted"][opt])
        if (sum(opt) > 1) {
            warning(paste("Multiple optimal cutpoints found, returning minimum of:",
                          paste(roccurve[, "x.sorted"][opt], collapse = ", ")))
        }
        m_oc <- max(m)
        res <- data.frame(oc, m_oc)
        colnames(res) <- c("optimal_cutpoint", metric_name)
        return(res)
    } else if (minmax == "min") {
        opt <- m == min(m)
        oc <- max(roccurve[, "x.sorted"][opt])
        if (sum(opt) > 1) {
            warning(paste("Multiple optimal cutpoints found, returning maximum of:",
                          paste(roccurve[, "x.sorted"][opt], collapse = ", ")))
        }
        m_oc <- min(m)
        res <- data.frame(oc, m_oc)
        colnames(res) <- c("optimal_cutpoint", metric_name)
        return(res)
    }
}

#' @export
maximize_metric <- function(data, x, class, metric_func = youden,
                            pos_class = NULL, neg_class = NULL,
                            direction) {
    metric_name <- as.character(substitute(metric_func))
    optimize_metric(data = data, x = x, class = class, metric_func = metric_func,
                    pos_class = pos_class, neg_class = neg_class, minmax = "max",
                    direction = direction, metric_name = metric_name)
}


#' @export
minimize_metric <- function(data, x, class, metric_func = youden,
                            pos_class = NULL, neg_class = NULL,
                            direction) {
    metric_name <- as.character(substitute(metric_func))
    optimize_metric(data = data, x = x, class = class, metric_func = metric_func,
                    pos_class = pos_class, neg_class = neg_class, minmax = "min",
                    direction = direction, metric_name = metric_name)
}



