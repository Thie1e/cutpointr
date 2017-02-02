#' @export
roc <- function(data, x, class,
                pos_class = NULL, neg_class = NULL, direction = ">=") {
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
        if (!(-Inf %in% x.sorted)) {
            x.sorted <- c(x.sorted, -Inf)
            class.sorted <- c(class.sorted, NA)
            tp <- c(tp, n_pos)
            fp <- c(fp, n_neg)
            tn <- c(tn, 0)
            fn <- c(fn, 0)
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

        if (!(Inf %in% x.sorted)) {
            x.sorted <- c(x.sorted, Inf)
            class.sorted <- c(class.sorted, NA)
            tp <- c(tp, n_pos)
            fp <- c(fp, n_neg)
            tn <- c(tn, 0)
            fn <- c(fn, 0)
        }
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
    return(data.frame(x.sorted, tp, fp, tn, fn, tpr, tnr))
}
