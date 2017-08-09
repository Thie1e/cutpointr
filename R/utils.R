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

find_metric_name_boot <- function(object) {
    if ("subgroup" %in% colnames(object)) {
        return(colnames(object)[4])
    } else {
        return(colnames(object)[3])
    }
}

check_colnames <- function(metric_name) {
    default_cols <- c("subgroup", "direction", "optimal_cutpoint",
                     "method", "accuracy", "sensitivity", "specificity", "AUC",
                     "pos_class", "neg_class", "prevalence",
                     "outcome", "predictor", "grouping", "data", "roc_curve")
    if (metric_name %in% default_cols)
        stop(paste("The metric function should return a matrix or data.frame",
                   "with a colname that differs from the default colnames of",
                   "cutpointr to avoid duplicates"))
}

validate_colnames <- function(coln) {
    valid_names <- make.names(coln)
    valid_names[valid_names == "NULL."] <- "NULL"
    invalid_names <- valid_names != coln
    if (any(invalid_names)) {
        warning(paste("Invalid column names:", coln[invalid_names]))
    }
}

ifel_pos_neg <- function(logi_vec, pos_class, neg_class) {
    predictions <- rep(neg_class, length(logi_vec))
    predictions[logi_vec] <- pos_class
    return(predictions)
}

midpoint <- function(oc, x, direction) {
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

# If the output of the metric function is no named matrix with one column,
# convert it to one. Also run some checks.
sanitize_metric <- function(m, m_name, n) {
    if ("data.frame" %in% class(m)) {
        m <- as.matrix(m)
    }
    if (!is.null(dim(m))) {
        if (dim(m)[2] == 1 & class(m) == "matrix") {
            res <- m
        } else {
            stop(paste("The metric function should return a numeric vector",
                       "or a one-column matrix or data.frame."))
        }
    } else if (is.numeric(m)) {
        res <- matrix(m, ncol = 1, dimnames = list(NULL, m_name))
    } else {
        stop(paste("Can't process metric of type", class(m)))
    }
    finite_res <- is.finite(res)
    if (any(!finite_res)) {
        message("Converting infinite metric values to NA")
        res[!finite_res] <- NA
    }
    if (nrow(res) != n) {
        stop("Number of returned metric values not equal to n")
    }
    colnames(res) <- make.names(colnames(res))
    return(res)
}


