
extract_opt_cut <- function(df) {
    optcut <- df$optimal_cutpoint
    if (!is.null(optcut)) return(optcut)
    return(df[1, 1])
}

find_metric_name <- function(colnames) {
        other_cols <- c("subgroup", "optimal_cutpoint", "Sens", "Spec", "direction",
                        "pos_class", "neg_class", "prevalence", "outcome",
                        "predictor", "grouping", "data", "boot")
        other_cols <- paste0(other_cols, collapse = "|")
        metric_name <- colnames[!grepl(pattern = other_cols, x = colnames)]
        metric_name <- metric_name[1] # If multiple metrics / other cols
        return(metric_name)
}

ifel_pos_neg <- function(logi_vec, pos_class, neg_class) {
    predictions <- rep(neg_class, length(logi_vec))
    predictions[logi_vec] <- pos_class
    return(predictions)
}

midpoint <- function(oc, x, direction) {
    x <- sort(unique(x))
    if (direction == ">=") {
        mean(c(oc, x[which(x == oc) + 1]))
    } else if (direction == "<=") {
        mean(c(oc, x[which(x == oc) - 1]))
    }
}
