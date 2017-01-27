# roccurve <- function(predictions, labels, pos.label, neg.label) {
#     pred.order <- order(predictions, decreasing = FALSE)
#     predictions.sorted <- predictions[pred.order]
#     tp <- cumsum(labels[pred.order] == pos.label)
#     fp <- cumsum(labels[pred.order] == neg.label)
#     # dups <- rev(duplicated(rev(predictions.sorted)))
#     # tp <- c(0, tp[!dups])
#     # fp <- c(0, fp[!dups])
#     # cutoffs <- c(Inf, predictions.sorted[!dups])
#     # return(list(cutoffs = cutoffs, fp = fp, tp = tp))
# }
#
# builtin_ecdf <- function(predictions, labels, pos_class, neg_class) {
#     neg_x <- predictions[labels == neg_class]
#     pos_x <- predictions[labels == pos_class]
#     fh <- stats::ecdf(neg_x)
#     gd <- stats::ecdf(pos_x)
# }

# temp <- data.frame(x = rnorm(5),
#                    y = sample(0:1, size = 5, replace = T))
#
# microbenchmark::microbenchmark(
#     roccurve(temp$x, labels = temp$y, 1, 0),
#     builtin_ecdf(temp$x, temp$y, 1, 0)
# )
#
# oc_youden2 <- function(data, predictions, labels,
#                       pos.label = NULL, neg.label = NULL, direction = ">=") {
#     data <- as.data.frame(data)
#     stopifnot(is.character(predictions))
#     stopifnot(is.character(labels))
#     labels <- data[, labels]
#     predictions <- data[, predictions]
#
#     if (direction == ">=") {
#         pred.order <- order(predictions, decreasing = TRUE)
#         predictions.sorted <- predictions[pred.order]
#         dups <- rev(duplicated(rev(predictions.sorted)))
#         predictions.sorted <- predictions.sorted[!dups]
#         labels.sorted <- labels[pred.order]
#         labels.sorted <- labels.sorted[!dups]
#         tp <- cumsum(labels.sorted == pos.label)
#         tp <- tp[!dups]
#         fp <- cumsum(labels.sorted == neg.label)
#         fp <- fp[!dups]
#         n_pos <- sum(labels == pos.label)
#         n_neg <- length(labels) - n_pos
#         tn <- n_neg - fp
#         fn <- n_pos + n_neg - tp - fp - tn
#
#         if (!(Inf %in% predictions.sorted)) {
#             predictions.sorted <- c(Inf, predictions.sorted)
#             labels.sorted <- c(NA, labels.sorted)
#             tp <- c(0, tp)
#             fp <- c(0, fp)
#             tn <- c(n_neg, tn)
#             fn <- c(n_pos, fn)
#         }
#         if (!(-Inf %in% predictions.sorted)) {
#             predictions.sorted <- c(predictions.sorted, -Inf)
#             labels.sorted <- c(labels.sorted, NA)
#             tp <- c(tp, n_pos)
#             fp <- c(fp, n_neg)
#             tn <- c(tn, 0)
#             fn <- c(fn, 0)
#         }
#
#         sens <- tp / n_pos
#         spec <- tn / n_neg
#
#         youden <- sens + spec - 1
#         opt <- youden == max(youden)
#
#         print(data.frame(labels.sorted, predictions.sorted, tp, fp, tn, fn, sens, spec, youden))
#
#         oc <- min(predictions.sorted[opt])
#         if (sum(opt) > 1) {
#             warning(paste("Multiple optimal cutpoints found, returning minimum of:",
#                           paste(predictions.sorted[opt], collapse = ", ")))
#         }
#         youden_oc <- max(youden)
#         return(data.frame(optimal_cutpoint = oc,
#                           youden           = youden_oc))
#     } else if (direction == "<=") {
#         pred.order <- order(predictions, decreasing = FALSE)
#         predictions.sorted <- predictions[pred.order]
#         dups <- rev(duplicated(rev(predictions.sorted)))
#         predictions.sorted <- predictions.sorted[!dups]
#         labels.sorted <- labels[pred.order]
#         tp <- cumsum(labels.sorted == pos.label)
#         tp <- tp[!dups]
#         fp <- cumsum(labels.sorted == neg.label)
#         fp <- fp[!dups]
#         n_pos <- sum(labels == pos.label)
#         n_neg <- length(labels) - n_pos
#         tn <- n_neg - fp
#         fn <- n_pos + n_neg - tp - fp - tn
#
#         if (!(Inf %in% predictions.sorted)) {
#             predictions.sorted <- c(predictions.sorted, Inf)
#             labels.sorted <- c(labels.sorted, NA)
#             tp <- c(tp, n_pos)
#             fp <- c(fp, n_neg)
#             tn <- c(tn, 0)
#             fn <- c(fn, 0)
#         }
#         if (!(-Inf %in% predictions.sorted)) {
#             predictions.sorted <- c(-Inf, predictions.sorted)
#             labels.sorted <- c(NA, labels.sorted)
#             tp <- c(0, tp)
#             fp <- c(0, fp)
#             tn <- c(n_neg, tn)
#             fn <- c(n_pos, fn)
#         }
#
#         sens <- tp / n_pos
#         spec <- tn / n_neg
#
#         youden <- sens + spec - 1
#         opt <- youden == max(youden)
#
#         print(data.frame(labels.sorted, predictions.sorted, tp, fp, tn, fn, sens, spec, youden))
#
#         oc <- min(predictions.sorted[opt])
#         if (sum(opt) > 1) {
#             warning(paste("Multiple optimal cutpoints found, returning minimum of:",
#                           paste(predictions.sorted[opt], collapse = ", ")))
#         }
#         youden_oc <- max(youden)
#         return(data.frame(optimal_cutpoint = oc,
#                           youden           = youden_oc))
#     }
# }
#

# temp <- data.frame(x = c(-Inf, 1, 2, 3, 4),
#                    y = c(1, 1, 0, 0, 0))
# oc_youden2(temp, "x", "y", 1, 0) # -Inf
# optimal.cutpoints(data = temp, X = "x", status = "y", tag.healthy = 0, methods = "Youden", direction = "<")
#
#
# temp <- data.frame(x = c(1, 2, 3, 4, Inf),
#                    y = c(0, 0, 0, 0, 1))
# oc_youden2(temp, "x", "y", 1, 0) # Inf
# optimal.cutpoints(data = temp, X = "x", status = "y", tag.healthy = 0, methods = "Youden", direction = "<")
#
#
# temp <- data.frame(x = c(1, 2, 3, 4, Inf),
#                    y = c(1, 1, 0, 0, 0))
# oc_youden2(temp, "x", "y", 1, 0, direction = "<=")
# optimal.cutpoints(data = temp, X = "x", status = "y", tag.healthy = 0, methods = "Youden", direction = ">")


