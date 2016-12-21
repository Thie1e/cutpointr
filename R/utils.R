
#### Evtl. einfacher nicht wirklich die midpoints einzufügen, sondern am Ende
#### mean(c(cutpoints[oc_ind], cutpoints[oc_ind + 1])) zurückzugeben

#' Insert midpoints between neighboring values after sorting, usually for cutpoints
# insert_midpoints <- function(x) {
#     midpoints <- na.omit(rowMeans(cbind(x, c(NA, x[-length(x)]))))
#     midpoints <- rowMeans(cbind(x, c(NA, x[-length(x)]))[-1, ])
#     ### write test to make sure there are no duplicates ####
#     lx <- length(x)
#     newx <- rep(NA, times = lx + lx - 1)
#     newx[seq(from = 1, by = 2, length.out = lx)] <- x
#     newx[seq(from = 2, by = 2, length.out = lx - 1)] <- midpoints
#     return(newx)
# }

# insert_midpoints2 <- function(x) {
#     lx <- length(x)
#     midpoints <- sapply(2:lx, function(i) {
#         mean(x[(i - 1):i])
#     })
#     newx <- rep(NA, times = lx + lx - 1)
#     newx[seq(from = 1, by = 2, length.out = lx)] <- x
#     newx[seq(from = 2, by = 2, length.out = lx - 1)] <- midpoints
#     return(newx)
# }

# x <- rnorm(100)
# microbenchmark::microbenchmark(
#     insert_midpoints(x),
#     insert_midpoints2(x),
#     times = 1000
# )
# Die 1. Funktion ist ca. 10-mal schneller


extract_opt_cut <- function(df) {
    optcut <- df$optimal_cutpoint
    if (!is.null(optcut)) return(optcut)
    return(df[1, 1])
}

find_metric_name <- function(colnames) {
        other_cols <- c("group", "optimal_cutpoint", "Sens", "Spec", "direction",
                        "pos_class", "neg_class", "prevalence", "outcome",
                        "predictor", "grouping", "data", "boot")
        other_cols <- paste0(other_cols, collapse = "|")
        metric_name <- colnames[!grepl(pattern = other_cols, x = colnames)]
        metric_name <- metric_name[1] # If multiple metrics / other cols
        return(metric_name)
}