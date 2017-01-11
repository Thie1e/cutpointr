#' Determine optimal cutpoint by maximizing the sum of sensitivity and specificity
#'
#' This function takes bivariate data of predictions or test values and class
#' labels and determines an optimal cutpoint to empirically maximize
#' sensitivity + specificity - 1 (the J-Index or Youden-Index).
#'
#' @inheritParams cutpointr
#' @importFrom purrr %>%
#' @return A data frame with one row, the column optimal_cutpoint and the column
#' youden
#' @examples
#' library(OptimalCutpoints)
#' data(elas)
#' oc_youden(elas, "elas", "status", pos_class = 1, neg_class = 0)
#' @export
oc_youden <- function(data, x, class,
                      candidate_cuts = unique(unlist(data[, x])),
                      pos_class = NULL, neg_class = NULL, direction = ">") {
    data <- as.data.frame(data)
    stopifnot(is.character(x))
    stopifnot(is.character(class))

    neg_x <- data[, x][data[, class] == neg_class]
    pos_x <- data[, x][data[, class] == pos_class]
    candidate_cuts <- inf_to_candidate_cuts(candidate_cuts, direction)

    #
    # Get optimal cutpoint
    #
    fh <- stats::ecdf(neg_x)
    gd <- stats::ecdf(pos_x)
    if (direction == ">") {
        youden <- fh(candidate_cuts) - gd(candidate_cuts)
        oc <- mean(candidate_cuts[youden == max(youden)])
        youden_oc <- fh(oc) - gd(oc)
        res <- data.frame(optimal_cutpoint = oc,
                          youden           = youden_oc)
    } else if (direction == "<") {
        youden <- gd(candidate_cuts) - fh(candidate_cuts)
        oc <- mean(candidate_cuts[youden == max(youden)])
        youden_oc <- gd(oc) - fh(oc)
        res <- data.frame(optimal_cutpoint = oc,
                          youden           = youden_oc)
    }
    return(res)
}

### Benchmarks vs. ROCR
# youden_rocr <- function(x,class){
#   pred<-ROCR::prediction(x,class)
#   perf<-ROCR::performance(pred, "sens", "spec")
#
#   cutpoint<-slot(perf, "alpha.values")[[1]]
#   spec<-slot(perf, "x.values")[[1]]
#   sens<-slot(perf, "y.values")[[1]]
#   res_tab<-data.frame(cutpoint, sens, spec)
#
#   oc <- res_tab[which.max(abs(res_tab$sens + res_tab$spec)),]
#
#   return(oc$cutpoint)
# }

# tempx <- rnorm(1000)
# tempy <- sample(c("a", "b"), size = 1000, replace = T)
# dat <- data.frame(value = tempx, class = tempy)
# microbenchmark::microbenchmark(
#     oc_youden.default(x = tempx, class = tempy, higher = F,
#                               pos_class = "a")
#     )
# microbenchmark::microbenchmark(
#     youden_rocr(x = tempx, class = tempy)
#     )
# microbenchmark::microbenchmark(
#     cutpointr(dat, value, class, pos_class = "a", higher = F)
#     )

# Check higher TRUE / FALSE
# tempx <- 1:7
# tempy <- c("a", "a", "a", "b", "b", "b", "b")
# cbind(tempx, tempy)
# oc_youden.default(tempx, tempy, pos_class = "a", higher = F)

