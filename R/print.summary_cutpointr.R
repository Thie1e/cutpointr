#' @export
print.summary_cutpointr <- function(x, ...) {
    cat(paste("Method:", x$cutpointr[[1]]$method, "\n"))
    cat(paste("Predictor:", x$cutpointr[[1]]$predictor, "\n"))
    cat(paste("Outcome:", x$cutpointr[[1]]$outcome, "\n"))
    cat(paste("Direction:", x$cutpointr[[1]]$direction, "\n"))
    if (has_column(x$cutpointr[[1]], "subgroup")) {
        cat(c("Subgroups:",
              paste(purrr::map(x$cutpointr, ~ .$subgroup), collapse = ", "),
              "\n"))
    }
    if (has_column(x, "boot")) {
        cat(paste("Nr. of bootstraps:", x$boot_runs[1], "\n"))
    }

    for (i in 1:nrow(x)) {
        cat("\n")
        if (has_column(x$cutpointr[[i]], "subgroup")) {
            cat(paste("Subgroup:", x$subgroup[i], "\n"))
            cat(paste0(rep("-", getOption("width")), collapse = ""), "\n")
        }
        purrr::map_df(1:length(x$cutpointr[[i]]$optimal_cutpoint[[1]]), function(j) {
            x$cutpointr[[i]] %>%
                dplyr::select_("optimal_cutpoint",
                               find_metric_name(x$cutpointr[[i]]),
                               "acc", "sensitivity",
                               "specificity", "AUC") %>%
                purrr::map_df(get_fnth, n = j) %>%
                round(digits = 4) %>%
                dplyr::mutate_(n_pos = ~ x$n_pos[i],
                               n_neg = ~ x$n_neg[i])
        }) %>%
            as.data.frame %>%
            print(row.names = FALSE)
        cat("\n")
        for (j in 1:nrow(x$confusion_matrix[[i]])) {
            cat(paste0("Cutpoint ", x$confusion_matrix[[i]]$cutpoint[j], ":"))
            cat("\n")
            cm <- unlist(x$confusion_matrix[[i]][j, 2:5])
            dim(cm) <- c(2,2)
            dimnames(cm) <- list(prediction = c(as.character(x$cutpointr[[i]]$pos_class),
                                                as.character(x$cutpointr[[i]]$neg_class)),
                                 observation = c(as.character(x$cutpointr[[i]]$pos_class),
                                                 as.character(x$cutpointr[[i]]$neg_class)))
            print(cm)
            cat("\n")
        }
        cat("\n")
        cat(paste("Predictor summary:", "\n"))
        print(x$desc[[i]])
        cat("\n")
        cat(paste("Predictor summary per class:", "\n"))
        print(x$desc_by_class[[i]])
        if (!is.null(suppressWarnings(x$boot[[i]]))) {
            cat("\n")
            cat(paste("Bootstrap summary:", "\n"))
            print(x$boot[[i]], row.names = rep("", nrow(x$boot[[i]])))
        }
    }
    return(invisible(x))
}

