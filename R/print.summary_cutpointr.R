#' @export
print.summary_cutpointr <- function(x, ...) {
    cat(paste("Method:", x$cutpointr[[1]]$method, "\n"))
    cat(paste("Predictor:", x$cutpointr[[1]]$predictor, "\n"))
    cat(paste("Outcome:", x$cutpointr[[1]]$outcome, "\n"))
    cat(paste("Direction:", x$cutpointr[[1]]$direction, "\n"))
    if (!is.null(suppressWarnings(x$cutpointr[[1]]$subgroup))) {
        cat(c("Subgroups:",
              paste(purrr::map(x$cutpointr, ~ .$subgroup), collapse = ", "),
              "\n"))
    }
    if (!is.null(suppressWarnings(x$boot))) {
        cat(paste("Nr. of bootstraps:", x$boot_runs[1], "\n"))
    }

    for (i in 1:nrow(x)) {
        cat("\n")
        if (!is.null(suppressWarnings(x$cutpointr[[i]]$subgroup))) {
            cat(paste("Subgroup:", x$subgroup[i], "\n"))
            cat(paste0(rep("-", getOption("width")), collapse = ""), "\n")
        }
        x$cutpointr[[i]] %>% dplyr::select_("optimal_cutpoint",
                                            find_metric_name(x$cutpointr[[i]]),
                                            "acc", "sensitivity",
                                            "specificity", "AUC") %>%
            round(digits = 4) %>%
            dplyr::mutate_(n_pos = ~ x$n_pos[i],
                           n_neg = ~ x$n_neg[i]) %>%
            data.frame(row.names = "") %>% print
        cat("\n")
        cm <- unlist(x$confusion_matrix[i])
        dim(cm) <- c(2,2)
        dimnames(cm) <- list(prediction = c(as.character(x$cutpointr[[i]]$pos_class),
                                            as.character(x$cutpointr[[i]]$neg_class)),
                             observation = c(as.character(x$cutpointr[[i]]$pos_class),
                                             as.character(x$cutpointr[[i]]$neg_class)))
        print(cm)
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
}

