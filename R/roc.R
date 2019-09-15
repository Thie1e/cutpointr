#' Calculate a ROC curve
#'
#' Given a \code{data.frame} with a numeric predictor variable and a binary outcome
#' variable this function returns a \code{data.frame} that includes all elements of
#' the confusion matrix (true positives, false positives, true negatives,
#' and false negatives) for every unique value of the predictor variable.
#' Additionally, the true positive rate (tpr), false positive rate (fpr),
#' true negative rate (tnr) and false negative rate (fnr) are returned.
#'
#' To enable classifying all observations as belonging to only one class the
#' predictor values will be augmented by Inf or -Inf. The returned object can
#' be plotted with plot_roc.
#'
#' This function uses tidyeval to support unquoted arguments. For programming
#' with \code{roc} the operator \code{!!} can be used to unquote an argument,
#' see the examples.
#'
#' @param data A data.frame or matrix. Will be converted to a data.frame.
#' @param x The name of the numeric predictor variable.
#' @param class The name of the binary outcome variable.
#' @param pos_class The value of 'class' that represents the positive cases.
#' @param neg_class The value of 'class' that represents the negative cases.
#' @param direction (character) One of ">=" or "<=". Specifies if the positive
#' class is associated with higher values of x (default).
#' @param silent If FALSE and the ROC curve contains no positives or negatives,
#' a warning is generated.
#' @return A data frame with the columns x.sorted, tp, fp, tn, fn, tpr, tnr, fpr,
#' and fnr.
#' @examples
#' roc_curve <- roc(data = suicide, x = dsi, class = suicide,
#'   pos_class = "yes", neg_class = "no", direction = ">=")
#' roc_curve
#' plot_roc(roc_curve)
#' auc(roc_curve)
#'
#' ## Unquoting an argument
#' myvar <- "dsi"
#' roc(suicide, x = !!myvar, suicide, pos_class = "yes", neg_class = "no")
#' @export
#' @family main cutpointr functions
#' @source
#' Forked from the \pkg{ROCR} package
roc <- function(data, x, class, pos_class, neg_class, direction = ">=",
                silent = FALSE) {
    stopifnot(direction %in% c(">=", "<="))
    data <- as.data.frame(data)

    x_sym <- rlang::ensym(x)
    x_expr <- rlang::enexpr(x_sym)
    x <- rlang::eval_tidy(expr = x_expr, data = data)
    class_sym <- rlang::ensym(class)
    class_expr <- rlang::enexpr(class_sym)
    class <- rlang::eval_tidy(expr = class_expr, data = data)

    if (direction == ">=") {
        pred.order <- order(x, decreasing = TRUE)
        x.sorted <- x[pred.order]
        dups <- rev(duplicated(rev(x.sorted)))
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
        dups <- rev(duplicated(rev(x.sorted)))
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
    res <- tibble::tibble(x.sorted, tp, fp, tn, fn, tpr, tnr, fpr, fnr)
    class(res) <- c("roc_cutpointr", class(res))
    if (!silent) {
        if (is.nan(res$tpr[1])) warning("ROC curve contains no positives")
        if (res$fpr[1] == 0 & res$fpr[nrow(res)] == 0) warning("ROC curve contains no negatives")
    }
    return(res)
}

