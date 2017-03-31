#' @export
summary.cutpointr <- function(object, ...) {
    x_summary <- vector("list", nrow(object))
    names(x_summary) <- suppressWarnings(object$subgroup)
    for (r in 1:nrow(object)) {
        temprow <- object[r, ]
        x_summary[[r]]$cutpointr <- temprow
        x_summary[[r]]$desc <- temprow$data[[1]] %>%
            dplyr::select_(as.name(temprow$predictor)) %>%
            unlist %>%
            summary_sd
        x_summary[[r]]$n_obs <- nrow(temprow$data[[1]])
        x_summary[[r]]$n_pos <- temprow$data[[1]] %>%
            dplyr::select_(as.name(temprow$outcome)) %>%
            unlist %>% (function(x) sum(x == temprow$pos_class))
        x_summary[[r]]$n_neg <- x_summary[[r]]$n_obs - x_summary[[r]]$n_pos
        # Confusion Matrix
        oi <- get_opt_ind(temprow$roc_curve[[1]], oc = temprow$optimal_cutpoint,
                          direction = temprow$direction)
        x_summary[[r]]$confusion_matrix <- unlist(temprow$roc_curve[[1]][oi, c("tp", "fn", "fp", "tn")])
        dim(x_summary[[r]]$confusion_matrix) <- c(2,2)
        dimnames(x_summary[[r]]$confusion_matrix) <- list(prediction = c(as.character(temprow$pos_class),
                                                                         as.character(temprow$neg_class)),
                                                          observation = c(as.character(temprow$pos_class),
                                                                          as.character(temprow$neg_class)))
        if (!is.null(suppressWarnings(temprow$boot))) {
            x_summary[[r]]$boot <- purrr::map_df(temprow$boot[[1]][, 1:10], function(x) {
                round(summary_sd(x), 4)
            })
            x_summary[[r]]$boot <- t(x_summary[[r]]$boot)
            colnames(x_summary[[r]]$boot) <- c("Min.", "1st Qu.", "Median", "Mean",
                                               "3rd Qu.", "Max", "SD")
        }
    }
    class(x_summary) <- c("summary_cutpointr")
    return(x_summary)
}

