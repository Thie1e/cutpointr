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
            dplyr::select(.data$direction,
                          .data$AUC) %>%
            # round(digits = digits) %>%
            dplyr::mutate(AUC = round(AUC, digits = digits),
                          n = x$n_obs[i],
                          n_pos = x$n_pos[i],
                          n_neg = x$n_neg[i]) %>%
            as.data.frame %>%
            print_df_nodat()

        cat("\n")

        purrr::map_df(1:length(x$cutpointr[[i]]$optimal_cutpoint[[1]]), function(j) {
            x$cutpointr[[i]] %>%
                dplyr::select(# .data$direction,
                              .data$optimal_cutpoint,
                              !!find_metric_name(x$cutpointr[[i]]),
                              .data$acc, .data$sensitivity,
                              .data$specificity) %>%
                purrr::map_df(get_fnth, n = j)
        }) %>%
            as.data.frame %>%
            dplyr::left_join(y = x$confusion_matrix[[i]],
                             by = c("optimal_cutpoint" = "cutpoint")) %>%
            dplyr::mutate_if(is.numeric, round, digits = digits) %>%
            print_df_nodat()

        cat("\n")

        cat(paste("Predictor summary:", "\n"))
        dplyr::bind_rows(x$desc[[i]], x$desc_by_class[[i]]) %>%
            dplyr::mutate_if(is.numeric, function(x) round(x, digits)) %>%
            print_df_nodat()
        if (has_boot_results(x[i, ])) {
            cat("\n")
            cat(paste("Bootstrap summary:", "\n"))
            print_df_nodat(
                x[["boot"]][[i]] %>%
                    dplyr::mutate_if(is.numeric, round, digits = 2),
                row.names = rep("", nrow(x[["boot"]][[i]]))
            )
        }

    }
    return(invisible(x))
}
