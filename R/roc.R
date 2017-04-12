#' Calculate a ROC curve
#'
#' Given a data frame with a numeric predictor variable and a binary outcome
#' variable this function returns a data frame that includes all elements of
#' a confusion matrix for every unique value of the predictor variable.
#' Additionally, the TPR, FPR, TNR and FNR are returned.
#' @param data A data frame or matrix. Will be converted to a data frame.
#' @param x (charaacter) The numeric independent (predictor) variable.
#' @param class (character) A binary vector of outcome values.
#' @param pos_class The value of 'class' that represents the positive cases.
#' @param neg_class The value of 'class' that represents the negative cases.
#' @param direction (character) One of ">=" or "<=". Specifies if the positive
#' class is associated with higher values of x (default).
#' @return A data frame with the columns x.sorted, tp, fp, tn, fn, tpr, tnr, fpr,
#' and fnr.
#' @examples
#' ## First two classes of the iris data
#' dat <- iris[1:100, ]
#' roc(data = dat, x = "Petal.Width", class = "Species",
#' pos_class = "versicolor", neg_class = "setosa", direction = ">=")
#' @export
roc <- function(data, x, class, pos_class, neg_class, direction = ">=") {
    stopifnot(direction %in% c(">=", "<="))
    data <- as.data.frame(data)
    stopifnot(is.character(x))
    stopifnot(is.character(class))
    class <- data[, class]
    x <- data[, x]

    if (direction == ">=") {
        pred.order <- order(x, decreasing = TRUE)
        x.sorted <- x[pred.order]
        dups <- rev(duplicated(rev(x.sorted)))
        x.sorted <- x.sorted[!dups]
        class.sorted <- class[pred.order]
        tp <- cumsum(class.sorted == pos_class)
        tp <- tp[!dups]
        fp <- cumsum(class.sorted == neg_class)
        fp <- fp[!dups]
        n_pos <- sum(class == pos_class)
        n_neg <- length(class) - n_pos
        tn <- n_neg - fp
        fn <- n_pos + n_neg - tp - fp - tn

        if (!(Inf %in% x.sorted)) {
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
        dups <- rev(duplicated(rev(x.sorted)))
        x.sorted <- x.sorted[!dups]
        class.sorted <- class[pred.order]
        tp <- cumsum(class.sorted == pos_class)
        tp <- tp[!dups]
        fp <- cumsum(class.sorted == neg_class)
        fp <- fp[!dups]
        n_pos <- sum(class == pos_class)
        n_neg <- length(class) - n_pos
        tn <- n_neg - fp
        fn <- n_pos + n_neg - tp - fp - tn

        if (!(-Inf %in% x.sorted)) {
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
    return(data.frame(x.sorted, tp, fp, tn, fn, tpr, tnr, fpr, fnr))
}
