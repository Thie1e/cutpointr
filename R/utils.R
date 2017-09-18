find_metric_name <- function(object) {
    if ("subgroup" %in% colnames(object)) {
        return(colnames(object)[5])
    } else {
        return(colnames(object)[4])
    }
}

find_metric_name_boot <- function(object) {
    if ("subgroup" %in% colnames(object)) {
        return(colnames(object)[6])
    } else {
        return(colnames(object)[5])
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

ensure_two_classes <- function(x) {
    uc <- unique(class)
    luc <- length(uc)
    if (luc != 2) stop(paste("Expecting two classes, got", luc))
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

only_one_unique <- function(x) {
    if (is.character(x) | is.factor(x)) {
        one_unique_char(x)
    } else {
        one_unique_num(x)
    }
}

which_cpp <- function(x, y) {
    if (is.numeric(x)) {
        return(which_are_num(x, y))
    } else {
        return(which_are_char(x, y))
    }
}

is_equal_cpp <- function(x, y) {
    if (is.numeric(x)) {
        return(is_equal_cpp_num(x, y))
    } else {
        return(is_equal_cpp_char(x, as.character(y)))
    }
}

na_inf_omit <- function(x) {
    x <- stats::na.omit(x)
    x <- x[is.finite(x)]
    return(x)
}
