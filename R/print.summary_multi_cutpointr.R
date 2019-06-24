#' @export
print.summary_multi_cutpointr <- function(x, digits = 4, ...) {
    cat(paste("Method:", x$cutpointr[[1]]$method, "\n"))
    cat(paste("Predictor:",
              paste(unique(purrr::map_chr(x$cutpointr, function(x) x$predictor)), collapse = ", "),
              "\n"))
    cat(paste("Outcome:", x$cutpointr[[1]]$outcome, "\n"))
    if (has_column(x$cutpointr[[1]], "subgroup")) {
        cat(c("Subgroups:",
              paste(unique(purrr::map(x$cutpointr, ~ .$subgroup)), collapse = ", "),
              "\n"))
    }
    if (has_boot_results(x)) {
        cat(paste("Nr. of bootstraps:", x$boot_runs[1], "\n"))
    }

    for (i in 1:nrow(x)) {
        cat("\n")
        cat(paste("Predictor:", x$cutpointr[[i]]$predictor, "\n"))
        if (has_column(x$cutpointr[[i]], "subgroup")) {
            cat(paste("Subgroup:", x$cutpointr[[i]]$subgroup, "\n"))
        }
        cat(paste0(rep("-", getOption("width")), collapse = ""), "\n")

        x$cutpointr[[i]] %>%
            dplyr::select(AUC) %>%
            round(digits = digits) %>%
            dplyr::mutate(n = x$n_obs[i],
                          n_pos = x$n_pos[i],
                          n_neg = x$n_neg[i]) %>%
            as.data.frame %>%
            print(row.names = FALSE)

        cat("\n")

        purrr::map_df(1:length(x$cutpointr[[i]]$optimal_cutpoint[[1]]), function(j) {
            x$cutpointr[[i]] %>%
                dplyr::select(direction,
                              optimal_cutpoint,
                              !!find_metric_name(x$cutpointr[[i]]),
                              acc, sensitivity,
                              specificity) %>%
                purrr::map_df(get_fnth, n = j)
        }) %>%
            as.data.frame %>%
            dplyr::left_join(y = x$confusion_matrix[[i]],
                             by = c("optimal_cutpoint" = "cutpoint")) %>%
            dplyr::mutate_if(is.numeric, round, digits = 4) %>%
            print(row.names = FALSE)

        cat("\n")

        # for (j in 1:nrow(x$confusion_matrix[[i]])) {
        #     cat(paste0("Cutpoint ", x$confusion_matrix[[i]]$cutpoint[j], ":"))
        #     cat("\n")
        #     cm <- unlist(x$confusion_matrix[[i]][j, 2:5])
        #     dim(cm) <- c(2,2)
        #     dimnames(cm) <- list(prediction = c(as.character(x$cutpointr[[i]]$pos_class),
        #                                         as.character(x$cutpointr[[i]]$neg_class)),
        #                          observation = c(as.character(x$cutpointr[[i]]$pos_class),
        #                                          as.character(x$cutpointr[[i]]$neg_class)))
        #     print(cm)
        #     cat("\n")
        # }
        # cat("\n")

        # cat(paste("Predictor summary:", "\n"))
        # print(x$desc[[i]])
        # cat("\n")
        # cat(paste("Predictor summary per class:", "\n"))
        # print(x$desc_by_class[[i]])
        # if (has_boot_results(x[i, ])) {
        #     cat("\n")
        #     cat(paste("Bootstrap summary:", "\n"))
        #     print.data.frame(x[["boot"]][[i]], row.names = rep("", nrow(x[["boot"]][[i]])))
        # }

        cat(paste("Predictor summary:", "\n"))
        rownames(x$desc[[i]]) <- "overall"
        print(round(rbind(x$desc[[i]], x$desc_by_class[[i]]), digits = digits))
        # print(x$desc[[i]])
        # cat("\n")
        # cat(paste("Predictor summary per class:", "\n"))
        # print(x$desc_by_class[[i]])
        if (has_boot_results(x[i, ])) {
            cat("\n")
            cat(paste("Bootstrap summary:", "\n"))
            print.data.frame(
                x[["boot"]][[i]] %>%
                    dplyr::mutate_if(is.numeric, round, digits = digits),
                # round(x[["boot"]][[i]], digits = round),
                row.names = rep("", nrow(x[["boot"]][[i]]))
            )
        }

    }
    return(invisible(x))
}
