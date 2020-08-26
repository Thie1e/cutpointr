#' @export
#' @importFrom tidyselect any_of
print.summary_cutpointr <- function(x, digits = 4, ...) {
    cat(paste("Method:", x$cutpointr[[1]]$method, "\n"))
    cat(paste("Predictor:", x$cutpointr[[1]]$predictor, "\n"))
    cat(paste("Outcome:", x$cutpointr[[1]]$outcome, "\n"))
    cat(paste("Direction:", x$cutpointr[[1]]$direction, "\n"))
    if (has_column(x$cutpointr[[1]], "subgroup")) {
        cat(c("Subgroups:",
              paste(purrr::map(x$cutpointr, ~ .$subgroup), collapse = ", "),
              "\n"))
    }
    if (has_boot_results(x)) {
        cat(paste("Nr. of bootstraps:", x$boot_runs[1], "\n"))
    }

    for (i in 1:nrow(x)) {
        cat("\n")
        if (has_column(x$cutpointr[[i]], "subgroup")) {
            cat(paste("Subgroup:", x$cutpointr[[i]]$subgroup, "\n"))
            cat(paste0(rep("-", getOption("width")), collapse = ""), "\n")
        }

        x$cutpointr[[i]] %>%
            dplyr::select(.data$AUC) %>%
            round(digits = digits) %>%
            dplyr::mutate(n = x$n_obs[i],
                          n_pos = x$n_pos[i],
                          n_neg = x$n_neg[i]) %>%
            as.data.frame %>%
            print(row.names = FALSE)

        cat("\n")

        purrr::map_df(1:length(x$cutpointr[[i]]$optimal_cutpoint[[1]]), function(j) {
            x$cutpointr[[i]] %>%
                # dplyr::select(-c(direction, method, AUC:boot)) %>%
                dplyr::select(!tidyselect::any_of(c("direction", "subgroup",
                                                    "method", "AUC",
                                                    "pos_class", "neg_class",
                                                    "prevalence", "outcome",
                                                    "predictor", "grouping",
                                                    "data", "roc_curve",
                                                    "boot"))) %>%
                purrr::map_df(get_fnth, n = j)
        }) %>%
            as.data.frame %>%
            dplyr::left_join(y = x$confusion_matrix[[i]],
                             by = c("optimal_cutpoint" = "cutpoint")) %>%
            round(digits = digits) %>%
            print(row.names = FALSE)

        cat("\n")

        cat(paste("Predictor summary:", "\n"))
        rownames(x$desc[[i]]) <- "overall"
        print(round(rbind(x$desc[[i]], x$desc_by_class[[i]]), digits = digits))
        if (has_boot_results(x[i, ])) {
            cat("\n")
            cat(paste("Bootstrap summary:", "\n"))
            print.data.frame(
                x[["boot"]][[i]] %>%
                    dplyr::mutate_if(is.numeric, round, digits = digits),
                row.names = rep("", nrow(x[["boot"]][[i]]))
            )
        }
    }
    return(invisible(x))
}

