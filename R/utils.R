extract_opt_cut <- function(df) {
    optcut <- df$optimal_cutpoint
    if (!is.null(optcut)) return(optcut)
    return(df[1, 1])
}

find_metric_name <- function(object) {
    if ("subgroup" %in% colnames(object)) {
        return(colnames(object)[5])
    } else {
        return(colnames(object)[4])
    }
}

check_colnames <- function(metric_name) {
    default_cols <- c("subgroup", "direction", "optimal_cutpoint",
                     "method", "accuracy", "sensitivity", "specificity", "AUC",
                     "pos_class", "neg_class", "prevalence",
                     "outcome", "predictor", "grouping", "data", "roc_curve")
    if (metric_name %in% default_cols)
        stop("The metric function should return a matrix with a colname that differs from the default colnames of cutpointr to avoid duplicates")
}

ifel_pos_neg <- function(logi_vec, pos_class, neg_class) {
    predictions <- rep(neg_class, length(logi_vec))
    predictions[logi_vec] <- pos_class
    return(predictions)
}

midpoint <- function(oc, x, direction) {
    #if (oc > max(x) | oc < min(x))
    x <- c(oc, x)
    if (direction == ">=") {
        x <- sort(unique(x))
    } else {
        x <- sort(unique(x), decreasing = TRUE)
    }
    if (direction == ">=") {
        mean(c(oc, x[utils::tail(which(x <= oc), 1) - 1]))
    } else if (direction == "<=") {
        mean(c(oc, x[utils::tail(which(x >= oc), 1) - 1]))
    }
}

get_opt_ind <- function(roc_curve, oc, direction) {
    if (direction == ">=") {
        opt_ind <- max(which(roc_curve$x.sorted >= oc))
    } else if (direction == "<=") {
        opt_ind <- max(which(roc_curve$x.sorted <= oc))
    }
    return(opt_ind)
}

summary_sd <- function(x) {
    c(summary(x)[1:6], SD = stats::sd(x, na.rm = TRUE))
}
