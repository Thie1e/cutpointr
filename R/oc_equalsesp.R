#' Find optimal cutpoint by minimizing the absolute difference between sensitivity and specificity
#'
#' @inheritParams cutpointr
#' @return A data frame with one row, the column optimal_cutpoint and the column
#' abs_d_sensspec
#' @examples
#' library(OptimalCutpoints)
#' data(elas)
#' oc_equalsesp(elas, "elas", "status", pos_class = 1, neg_class = 0, direction = ">")
#' @export
oc_equalsesp <- function(data, x, class,
                         candidate_cuts = unique(unlist(data[, x])),
                         pos_class = NULL, neg_class = NULL, direction = ">") {
    stopifnot(is.character(x))
    stopifnot(is.character(class))
    data <- as.data.frame(data)

    neg_x <- data[, x][data[, class] == neg_class]
    pos_x <- data[, x][data[, class] == pos_class]
    candidate_cuts <- inf_to_candidate_cuts(candidate_cuts, direction)

    if (direction == ">") {
        fh <- stats::ecdf(neg_x)
        gd <- stats::ecdf(pos_x)
        sens_c <- 1 - gd(candidate_cuts)
        spec_c <- fh(candidate_cuts)
        abs_d_sesp <- abs(sens_c - spec_c)
        oc <- mean(candidate_cuts[abs_d_sesp == min(abs_d_sesp)])
        abs_d_sensspec_oc <- abs(1 - gd(oc) - fh(oc))
        res <- data.frame(optimal_cutpoint = oc,
                          abs_d_sensspec   = abs_d_sensspec_oc)
    } else if (direction == "<") {
        fh <- stats::ecdf(-neg_x)
        gd <- stats::ecdf(-pos_x)
        sens_c <- 1 - gd(-candidate_cuts)
        spec_c <- fh(-candidate_cuts)
        abs_d_sesp <- abs(sens_c - spec_c)
        oc <- mean(-candidate_cuts[abs_d_sesp == min(abs_d_sesp)])
        abs_d_sensspec_oc <- abs(1 - gd(oc) - fh(oc))
        oc <- -oc
        res <- data.frame(optimal_cutpoint = oc,
                          abs_d_sensspec   = abs_d_sensspec_oc)
    }
    return(res)
}

### Benchmarks vs. various other ways of computing equal_sesp

# equal_sen_spec<-function(x,class, neg_class, pos_class){
#   pred<-ROCR::prediction(x,class, label.ordering = c(neg_class, pos_class))
#   perf<-ROCR::performance(pred, "sens", "spec")
#
#   cutpoint<-slot(perf, "alpha.values")[[1]]
#   spec<-slot(perf, "x.values")[[1]]
#   sens<-slot(perf, "y.values")[[1]]
#   res_tab<-data.frame(cutpoint, sens, spec)
#   # print(res_tab)
#
#   oc <- res_tab[which(abs(res_tab$sens - res_tab$spec) == min(abs(res_tab$sens - res_tab$spec))),]
#
#   return(oc)
# }


# oc_sesp_simple <- function(x, class, pos_class, higher) {
#     candidate_cuts <- unique(x)
#     if (higher) {
#         candidate_cuts <- unique(c(-Inf, candidate_cuts))
#     } else {
#         candidate_cuts <- unique(c(candidate_cuts, Inf))
#     }
#
#     abs_d_sensspec <- matrix(NA, ncol = 5, nrow = length(candidate_cuts))
#     colnames(abs_d_sensspec) <- c("d_sesp", "TP", "TN", "sens", "spec")
#
#     sumPos <- sum(class == pos_class)
#     sumNeg <- sum(class != pos_class)
#
#     if (higher) {
#         for (i in 1:length(candidate_cuts)) {
#             cu <- candidate_cuts[i]
#             TP <- sum((x > cu) * (class == pos_class))
#             TN <- sum((x <= cu) * (class != pos_class))
#             sens <- TP / sumPos
#             spec <- TN / sumNeg
#             abs_d_sensspec[i, ] <- c(abs(sens - spec), TP, TN, sens, spec)
#         }
#     } else {
#         for (i in 1:length(candidate_cuts)) {
#             cu <- candidate_cuts[i]
#             TP <- sum((x < cu) * (class == pos_class))
#             TN <- sum((x >= cu) * (class != pos_class))
#             sens <- TP / sumPos
#             spec <- TN / sumNeg
#             abs_d_sensspec[i, ] <- c(abs(sens - spec), TP, TN, sens, spec)
#         }
#     }
#
#     # print(data.frame(c = candidate_cuts,
#     #                  abs_d_sensspec))
#
#     oc_ind <- which(abs_d_sensspec[, "d_sesp"] == min(abs_d_sensspec[, "d_sesp"]))
#     if (length(oc_ind) > 1) warning("multiple optimal cuts found")
#     data.frame(oc = mean(candidate_cuts[oc_ind]),
#                d_sesp = min(abs_d_sensspec[, "d_sesp"]))
# }

# unnorm_roc <- function(x, class, pos_class) {
#     pred.order <- order(x, decreasing = TRUE)
#     predictions.sorted <- x[pred.order]
#     tp <- cumsum(class[pred.order] == pos_class)
#     fp <- cumsum(class[pred.order] != pos_class)
#     dups <- rev(duplicated(rev(predictions.sorted)))
#     # >= ? ---------------------------------------
#     tp <- c(0, tp[!dups])
#     fp <- c(0, fp[!dups])
#     cutoffs <- c(Inf, predictions.sorted[!dups])
#     # -----------------------------------------
#     # > ? --------------------------------------
#     # tp <- tp[!dups]
#     # fp <- fp[!dups]
#     # cutoffs <- predictions.sorted[!dups]
#     # ----------------------------------------
#     return(list(cutoffs = cutoffs, fp = fp, tp = tp))
# }

# eq_sesp_rocr <- function(x, class, pos_class) {
#     ###### Hier wachsen doch n.pos usw. im Loop?!
#     # Die Liste ist wohl für Fälle gedacht, in denen x eine Matrix ist
#     x <- list(x)
#     class <- list(class)
#     cutoffs <- list()
#     fp <- list()
#     tp <- list()
#     fn <- list()
#     tn <- list()
#     n.pos <- list()
#     n.neg <- list()
#     n.pos.pred <- list()
#     n.neg.pred <- list()
#     for (i in 1:length(x)) {
#         n.pos <- c(n.pos, sum(class[[i]] == pos_class))
#         n.neg <- c(n.neg, sum(class[[i]] != pos_class))
#         ans <- unnorm_roc(x[[i]], class[[i]], pos_class = pos_class)
#         cutoffs <- c(cutoffs, list(ans$cutoffs))
#         fp <- c(fp, list(ans$fp))
#         tp <- c(tp, list(ans$tp))
#         fn <- c(fn, list(n.pos[[i]] - tp[[i]]))
#         tn <- c(tn, list(n.neg[[i]] - fp[[i]]))
#         n.pos.pred <- c(n.pos.pred, list(tp[[i]] + fp[[i]]))
#         n.neg.pred <- c(n.neg.pred, list(tn[[i]] + fn[[i]]))
#     }
#     sens <- tp[[1]] / n.pos[[1]]
#     spec <- tn[[1]] / n.neg[[1]]
#     d_sesp <- abs(sens - spec)
#     oc_ind <- which(d_sesp == min(d_sesp))
#     if (length(oc_ind) > 1) warning("multiple optimal cuts found")
#     data.frame(oc = mean(cutoffs[[1]][oc_ind]),
#                d_sesp = min(d_sesp)) # d_sesp stimmt bei mehreren cutoffs nicht unbedingt
# }


# Check higher TRUE / FALSE
# tempx <- 1:7
# tempy <- c("a", "a", "a", "b", "b", "b", "b")
# cbind(tempx, tempy)
# equal_sen_spec(tempx, tempy, neg_class = "a", "b") # ROCR benutzt implizit >=
# oc_equalsesp.default(tempx, tempy, higher = T, pos_class = "b")
# oc_sesp_simple(tempx, tempy, higher = T, pos_class = "b")
# eq_sesp_rocr(tempx, tempy, pos_class = "b")
#
#
# tempx <- sort(rnorm(100000))
# tempy <- sample(c("a", "b"), size = 100000, replace = T)
# microbenchmark::microbenchmark(
#     # optcut_emp_youden.default(tempx, tempy, pos_class = "b", higher = T),
#     equal_sen_spec(tempx, tempy, "a", "b"), # ROCR benutzt implizit >=
#     oc_equalsesp.default(tempx, tempy, higher = T, pos_class = "b"),
#     # oc_sesp_simple(x = tempx, class = tempy, higher = T, pos_class = "b"),
#     eq_sesp_rocr(tempx, tempy, pos_class = "b"),
#     times = 20
# )

