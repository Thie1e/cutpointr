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

default_cols <- c("m", "subgroup", "direction", "optimal_cutpoint",
                  "method", "acc", "sensitivity", "specificity", "AUC",
                  "pos_class", "neg_class", "prevalence",
                  "outcome", "predictor", "grouping", "data", "roc_curve",
                  "boot", "tn", "fn", "tp", "fp", "tpr", "tnr", "fpr", "fnr")

check_method_cols <- function(method_result) {
    cn <- colnames(method_result)
    n_col <- ncol(method_result)
    identified_cols <- 0
    oc_col <- which(cn == "optimal_cutpoint")
    if (!is.null(oc_col)) identified_cols <- identified_cols + 1
    if ("roc_curve" %in% cn) {
        roc_col <- which(cn == "roc_curve")
        identified_cols <- identified_cols + 1
    }
    if (identified_cols == 1 & n_col == 2) {
        metric_col <- (1:n_col)[-oc_col]
    } else if (identified_cols < n_col) {
        stopifnot(exists("roc_col"))
        metric_col <- (1:n_col)[-c(oc_col, roc_col)]
        if (length(metric_col) >= 2) {
            stop(paste("method function returned too many columns.",
                       "Should return optimal_cutpoint, roc_curve (optional)",
                       "and a metric column (optional)."))
        }
        metric_name <- cn[metric_col]
        if (metric_name %in% default_cols) {
            colnames(method_result)[metric_col] <- paste0("metric_", metric_name)
        }
    }
    return(method_result)
}

check_metric_name <- function(met) {
    # Numeric vector
    if (!is.array(met) & is.numeric(met)) return(met)
    cn <- colnames(met)
    if (cn %in% default_cols) {
        colnames(met) <- paste0("metric_", cn)
        return(met)
    } else {
        return(met)
    }
}

check_colnames <- function(cutpointr_object) {
    if ("subgroup" %in% colnames(cutpointr_object)) col_nr <- 4 else col_nr <- 3
    metric_name <- colnames(cutpointr_object)[col_nr]
    if (metric_name %in% default_cols) {
        metric_name2 <- paste0("metric_", metric_name)
        colnames(cutpointr_object)[col_nr] <- metric_name2
        cutpointr_object$metric_name <- metric_name2
    } else {
        cutpointr_object$metric_name <- metric_name
    }
    return(cutpointr_object)
}

check_roc_curve <- function(object) {
    if (!("roc_cutpointr" %in% class(object$roc_curve[[1]]))) {
        stop(paste("roc_curve as returned by the method function is not an",
                   "object of the class roc_cutpointr"))
    }
}

has_column <- function(x, colname) {
    if (colname %in% colnames(x) | colname %in% names(x)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

has_boot_results <- function(x) {
    if (has_column(x, "boot")) {
        if (all(is.na(x[["boot"]]))) {
            return(FALSE)
        } else {
            return(TRUE)
        }
    } else {
        return(FALSE)
    }
}

ifel_pos_neg <- function(logi_vec, pos_class, neg_class) {
    predictions <- rep(neg_class, length(logi_vec))
    predictions[logi_vec] <- pos_class
    return(predictions)
}

get_fnth <- function(x, n = 1) {
    x <- unlist(x)
    if (length(x) == 1) {
        return(x[1])
    } else {
        return(x[n])
    }
    stop("no conditions apply in get_fnth")
}

midpoint <- function(oc, x, direction) {
    sapply(oc, function(oc) {
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
    })
}

apply_break_ties <- function(oc, f) {
    stopifnot(nrow(oc) == 1)
    optimal_cutpoint <- f(oc[["optimal_cutpoint"]][[1]])
    if (length(optimal_cutpoint) > 1) {
        optimal_cutpoint <- list(optimal_cutpoint)
    }
    oc$optimal_cutpoint <- optimal_cutpoint
    return(oc)
}

get_opt_ind <- function(roc_curve, oc, direction) {
    stopifnot(is.numeric(oc) | is.na(oc))
    sapply(oc, function(x) {
        if (direction == ">=") {
            opt_ind <- max(which(roc_curve$x.sorted >= x))
        } else if (direction == "<=") {
            opt_ind <- max(which(roc_curve$x.sorted <= x))
        }
        return(opt_ind)
    })
}

summary_sd <- function(x, round_digits = 4) {
    x <- unlist(x)
    s <- summary(x)[1:6]
    result <- c(s[1],
                stats::quantile(x, 0.05, na.rm = TRUE),
                s[2:5],
                stats::quantile(x, 0.95, na.rm = TRUE),
                s[6],
                SD = stats::sd(x, na.rm = TRUE))
    round(result, round_digits)
}

# If the output of the metric function is no named matrix with one column,
# convert it to one. Also run some checks.
sanitize_metric <- function(m, m_name, n, silent = TRUE) {
    if ("data.frame" %in% class(m)) {
        m <- as.matrix(m)
    }
    if (!is.null(dim(m))) {
        if (dim(m)[2] == 1 & class(m) == "matrix") {
            res <- m
            if (is.null(colnames(res))) colnames(res) <- m_name
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
        if (!silent) message("Converting non-finite metric values to NA")
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

.onUnload <- function (libpath) {
    library.dynam.unload("cutpointr", libpath)
}

add_list <- function(x, y, name) {
    if (length(y) > 1) {
        x[[name]] <- list(y)
    } else {
        x[[name]] <- y
    }
    return(x)
}

# rbind list elements that are tibbles with different column types (list or dbl,
# may be necessary for bootstrap results, if only one bootstrap resulted in
# multiple optimal cutpoints)
# Convert non-list columns to list so that bind_rows doesn't complain
prepare_bind_rows <- function(x) {
    stopifnot(is.list(x))
    if (length(x) < 2) {
        return(x)
    } else {
        list_cols <- purrr::map(x, function(x) {
            which(purrr::map_chr(x, class) == "list")
        })
        list_cols <- unique(unlist(list_cols))
        x <- purrr::map(x, function(x) {
            dplyr::mutate_at(.tbl = x, .vars = list_cols, .funs = function(x) {
                if (!is.list(x)) {
                    purrr::map(x, function(x) x)
                } else {
                    x
                }
            })
        })
        return(x)
    }
}

# Draw a bootstrap sample from a data frame. Draw again, if the sampled or
# unsampled observations contain only one class.
# simple_boot <- function(data, dep_var) {
#     draw_again <- TRUE
#     i <- 1
#     while (draw_again) {
#         b_ind <- sample(1:nrow(data), size = nrow(data), replace = TRUE)
#         if (only_one_unique(unlist(data[b_ind, dep_var])) |
#             only_one_unique(unlist(data[-b_ind, dep_var]))) {
#             draw_again <- TRUE
#             i <- i + 1
#             if (i >= 100) stop(paste("No sets including both classes drawn in",
#                                      "bootstrap after 100 tries."))
#         } else {
#             draw_again <- FALSE
#         }
#     }
#     return(b_ind)
# }

# Return indices for observations based on nonparametric bootstrap per class.
# If not per_class, draw a bootstrap sample and draw again, if the sampled or
# observations contain only one class. Preliminary tests suggested that
# per_class = FALSE leads to better confidence intervals.
simple_boot <- function(ind_pos = NULL, ind_neg = NULL,
                        data = NULL, dep_var = NULL,
                        per_class = FALSE) {
    if (per_class) {
        b_ind_pos <- sample(ind_pos, size = length(ind_pos), replace = TRUE)
        b_ind_neg <- sample(ind_neg, size = length(ind_neg), replace = TRUE)
        return(c(b_ind_pos, b_ind_neg))
    } else {
        draw_again <- TRUE
        i <- 1
        while (draw_again) {
            b_ind <- sample(1:nrow(data), size = nrow(data), replace = TRUE)
            if (only_one_unique(unlist(data[b_ind, dep_var])) |
                only_one_unique(unlist(data[-b_ind, dep_var]))) {
                draw_again <- TRUE
                i <- i + 1
                if (i >= 100) stop(paste("No sets including both classes drawn in",
                                         "bootstrap after 100 tries."))
            } else {
                draw_again <- FALSE
            }
        }
        return(b_ind)
    }
}


