#' @export
print.summary_cutpointr <- function(x, ...) {
    cat(paste("Method:", x[[1]]$cutpointr$method, "\n"))
    cat(paste("Predictor:", x[[1]]$cutpointr$predictor, "\n"))
    cat(paste("Outcome:", x[[1]]$cutpointr$outcome, "\n"))
    cat(paste("Direction:", x[[1]]$cutpointr$direction, "\n"))
    if (!is.null(suppressWarnings(x[[1]]$cutpointr$subgroup))) {
        cp <- purrr::transpose(x)
        sg <- do.call(rbind, cp$cutpointr)$subgroup
        sg <- paste(sg, collapse = ", ")
        cat(paste("Subgroups:", sg), "\n")
    }
    if (!is.null(suppressWarnings(x[[1]]$boot))) {
        cat(paste("Nr. of bootstraps:", nrow(x[[1]]$boot), "\n"))
    }

    for (i in 1:length(x)) {
        cat("\n")
        cat(paste(names(x)[i], "\n"))
        cat(paste0(rep("-", getOption("width")), collapse = ""), "\n")
        x[[i]]$cutpointr %>% dplyr::select_("optimal_cutpoint",
                                            find_metric_name(x[[i]]$cutpointr),
                                            "accuracy", "sensitivity",
                                            "specificity", "AUC") %>%
            round(digits = 4) %>%
            dplyr::mutate_(n_pos = ~ x[[i]]$n_pos,
                           n_neg = ~ x[[i]]$n_neg) %>%
            data.frame(row.names = "") %>% print
        cat("\n")
        print(x[[i]]$confusion_matrix)
        cat("\n")
        cat(paste("Predictor summary:", "\n"))
        print(x[[i]]$desc)
        if (!is.null(suppressWarnings(x[[i]]$boot))) {
        cat("\n")
        cat(paste("Bootstrap summary:", "\n"))
        print(x[[i]]$boot)
        }
    }
}

