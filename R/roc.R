#' Calculate a ROC curve
#'
#' Given a data frame with a numeric predictor variable and a binary outcome
#' variable this function returns a data frame that includes all elements of
#' the confusion matrix (true positives, false positives, true negatives,
#' and false negatives) for every unique value of the predictor variable.
#' Additionally, the true positive rate (tpr), false positive rate (fpr),
#' true negative rate (tnr) and false negative rate (fnr) are returned.
#'
#' To enable classifying all observations as belonging to only one class the
#' predictor values will be augmented by Inf or -Inf.
#'
#' @param data A data frame or matrix. Will be converted to a data frame.
#' @param x (character) The numeric independent (predictor) variable.
#' @param class (character) A binary vector of outcome values.
#' @param pos_class The value of 'class' that represents the positive cases.
#' @param neg_class The value of 'class' that represents the negative cases.
#' @param direction (character) One of ">=" or "<=". Specifies if the positive
#' class is associated with higher values of x (default).
#' @param silent If FALSE and the ROC curve contains no positives or negatives,
#' a warning is generated.
#' @return A data frame with the columns x.sorted, tp, fp, tn, fn, tpr, tnr, fpr,
#' and fnr.
#' @examples
#' ## First two classes of the iris data
#' dat <- iris[1:100, ]
#' roc(data = dat, x = "Petal.Width", class = "Species",
#' pos_class = "versicolor", neg_class = "setosa", direction = ">=")
#' @export
#' @source
#' Forked from the ROCR package
roc <- function(data, x, class, pos_class, neg_class, direction = ">=",
                silent = FALSE) {
    stopifnot(direction %in% c(">=", "<="))
    data <- as.data.frame(data)
    stopifnot(is.character(x))
    stopifnot(is.character(class))
    class <- data[, class]
    x <- data[, x]

    if (direction == ">=") {
        pred.order <- order(x, decreasing = TRUE)
        x.sorted <- x[pred.order]
        dups <- get_rev_dups(x.sorted)
        x.sorted <- x.sorted[!dups]
        class.sorted <- class[pred.order]
        tp <- cumsum(is_equal_cpp(class.sorted, pos_class))
        tp <- tp[!dups]
        fp <- cumsum(is_equal_cpp(class.sorted, neg_class))
        fp <- fp[!dups]
        n_pos <- tp[length(tp)]
        n_neg <- length(class) - n_pos
        tn <- n_neg - fp
        fn <- n_pos + n_neg - tp - fp - tn

        if (!(any_inf(x.sorted))) {
            x.sorted <- c(Inf, x.sorted)
            class.sorted <- c(NA, class.sorted)
            tp <- c(0, tp)
            fp <- c(0, fp)
            tn <- c(n_neg, tn)
            fn <- c(n_pos, fn)
        }
    } else if (direction == "<=") {
        pred.order <- order(x, decreasing = FALSE)
        x.sorted <- x[pred.order]
        dups <- get_rev_dups(x.sorted)
        x.sorted <- x.sorted[!dups]
        class.sorted <- class[pred.order]
        tp <- cumsum(is_equal_cpp(class.sorted, pos_class))
        tp <- tp[!dups]
        fp <- cumsum(is_equal_cpp(class.sorted, neg_class))
        fp <- fp[!dups]
        n_pos <- tp[length(tp)]
        n_neg <- length(class) - n_pos
        tn <- n_neg - fp
        fn <- n_pos + n_neg - tp - fp - tn

        if (!(any_inf(x.sorted))) {
            x.sorted <- c(-Inf, x.sorted)
            class.sorted <- c(NA, class.sorted)
            tp <- c(0, tp)
            fp <- c(0, fp)
            tn <- c(n_neg, tn)
            fn <- c(n_pos, fn)
        }
    }
    tpr <- tp / n_pos
    tnr <- tn / n_neg
    fpr <- 1 - tnr
    fnr <- 1 - tpr
    res <- data.frame(x.sorted, tp, fp, tn, fn, tpr, tnr, fpr, fnr)
    class(res) <- c(class(res), "roc_cutpointr")
    if (is.nan(res$tpr[1])) warning("ROC curve contains no positives")
    if (res$fpr[1] == 0 & res$fpr[nrow(res)] == 0) warning("ROC curve contains no negatives")
    return(res)
}
