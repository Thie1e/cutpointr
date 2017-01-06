#' @export
gather_cutoffs <- function(data, x, class,  candidate_cuts = unique(data[, x]),
                           metric_func = NULL, pos_class, higher = NULL) {
    ###
    # NSE?
    stopifnot(is.character(x))
    stopifnot(is.character(class))
    ###

    ### Prep
    if (is.null(higher)) higher = T
    ####

    metric_name <- as.character(match.call()$metric_func)
    candidate_cuts <- sort(candidate_cuts)

    neg_class <- unique(data[, class])
    neg_class <- neg_class[neg_class != pos_class]
    metrics <- purrr::map_dbl(candidate_cuts, function(cutpoint) {
        p <- ifelse(data[, x] > cutpoint, pos_class, neg_class)
        metric_func(preds = p, obs = data[, class], pos_class)
    })
    res <- data.frame(cutpoint = candidate_cuts, metrics)
    colnames(res)[2] <- metric_name
    return(res)
}

