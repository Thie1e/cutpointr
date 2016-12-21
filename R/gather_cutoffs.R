
gather_cutoffs <- function(x, class,  candidate_cuts = unique(x),
                           metric_func = NULL, pos_class = NULL, higher = NULL) {
    #
    # Preparation ---------
    #
    if (!is.null(higher) && higher == F) stop("higher = F not yet implemented")
    if (!is.factor(class)) class <- as.factor(class)
    if (length(levels(class)) != 2) {
        stop(paste("Expecting two classes, got", length(levels(class))))
    }
    if (is.null(pos_class)) {
        pos_class <- levels(class)[1]
        message(paste("Assuming", pos_class, "as positive class"))
    }
    stopifnot(pos_class %in% levels(class))
    # args <- as.list(match.call()[-1])
    # if (!("higher" %in% names(args))) {
    #     if (args$higher) {
    #         message(paste("Assuming higher x values imply positive class"))
    #     }
    # }
    neg_x <- x[class != pos_class]
    pos_x <- x[class == pos_class]
    if (is.null(higher)) {
        if (mean(neg_x) < mean(pos_x)) {
            message("Assuming the positive class has higher x values")
            higher = T
        } else {
            message("Assuming the positive class has lower x values")
            stop("higher = F not yet implemented")
            higher = F
        }
    }
    #
    # End preparation -------
    #
    metric_name <- as.character(match.call()$metric_func)

    # Assuming higher = T
    # p <- ifelse(x > candidate_cuts, pos_class, 0)
    metrics <- purrr::map_dbl(candidate_cuts, function(cutpoint) {
        p <- ifelse(cutpoint > x, pos_class, 0)
        metric_func(preds = p, obs = class, pos_class)
    })
    res <- data.frame(cutpoint = candidate_cuts, metrics)
    colnames(res)[2] <- metric_name
    # class(res) <- c("cp_result", "data.frame") # Klasse cp_result wird in cutpointr gesetzt
    return(res)
}

