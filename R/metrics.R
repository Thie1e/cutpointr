
sens_spec <- function(tp, fp, tn, fn) {
    sens <- tp / (tp + fn)
    spec <- tn / (tn + fp)
    res <- cbind(sens, spec)
    colnames(res) <- c("Sensitivity", "Specificity")
    return(res)
}

sesp_from_oc <- function(roc_curve, oc, direction, opt_ind = NULL) {
    if (is.null(opt_ind)) {
        opt_ind <- get_opt_ind(roc_curve = roc_curve, oc = oc, direction = direction)
    }
    sens_spec(tp = roc_curve$tp[opt_ind], fp = roc_curve$fp[opt_ind],
              tn = roc_curve$tn[opt_ind], fn = roc_curve$fn[opt_ind])
}

accuracy_from_oc <- function(roc_curve, oc, direction, opt_ind = NULL) {
    if (is.null(opt_ind)) {
        opt_ind <- get_opt_ind(roc_curve = roc_curve, oc = oc, direction = direction)
    }
    accuracy(tp = roc_curve$tp[opt_ind], fp = roc_curve$fp[opt_ind],
              tn = roc_curve$tn[opt_ind], fn = roc_curve$fn[opt_ind])
}

kappa_from_oc <- function(roc_curve, oc, direction, opt_ind = NULL) {
    if (is.null(opt_ind)) {
        opt_ind <- get_opt_ind(roc_curve = roc_curve, oc = oc, direction = direction)
    }
    cohens_kappa(tp = roc_curve$tp[opt_ind], fp = roc_curve$fp[opt_ind],
              tn = roc_curve$tn[opt_ind], fn = roc_curve$fn[opt_ind])
}

#' @source Forked from the AUC package
auc <- function(tpr, fpr) {
    l_tpr <- length(tpr)
    l_fpr <- length(fpr)
    stopifnot(l_tpr == l_fpr)
    tpr <- cbind(tpr[2:l_tpr], tpr[1:(l_tpr - 1)])
    fpr <- cbind(fpr[2:l_fpr], fpr[1:(l_fpr - 1)])
    sum(0.5 * abs(fpr[, 1] - fpr[, 2]) * (tpr[, 1] + tpr[, 2]))
}


#' Calculate accuracy
#' Calculate accuracy from the elements of a confusion matrix, that is
#' true positives, false positives, true negatives and false negatives.
#' The inputs must be vectors of equal length.
#' @param tp (numeric) number of true positives.
#' @param fp (numeric) number of false positives.
#' @param tn (numeric) number of true negatives.
#' @param fn (numeric) number of false negatives.
#' @examples
#' accuracy(10, 5, 20, 10)
#' @export
accuracy <- function(tp, fp, tn, fn) {
    Accuracy = cbind((tp + tn) / (tp + fp + tn + fn))
    colnames(Accuracy) <- "Accuracy"
    return(Accuracy)
}

#' Calculate the Youden-Index
#' Calculate the Youden-Index (J-Index) from the elements of a confusion matrix,
#' that is true positives, false positives, true negatives and false negatives.
#' The inputs must be vectors of equal length.
#' @param tp (numeric) number of true positives.
#' @param fp (numeric) number of false positives.
#' @param tn (numeric) number of true negatives.
#' @param fn (numeric) number of false negatives.
#' @examples
#' youden(10, 5, 20, 10)
#' @export
youden <- function(tp, fp, tn, fn) {
    sesp <- sens_spec(tp, fp, tn, fn)
    youden <- cbind(rowSums(sesp) - 1)
    colnames(youden) <- "Youden_Index"
    return(youden)
}

#' Calculate the sum of sensitivity and specificity
#' Calculate the sum of sensitivity and specificity from the elements of a confusion matrix,
#' that is true positives, false positives, true negatives and false negatives.
#' The inputs must be vectors of equal length.
#' @param tp (numeric) number of true positives.
#' @param fp (numeric) number of false positives.
#' @param tn (numeric) number of true negatives.
#' @param fn (numeric) number of false negatives.
#' @examples
#' sum_sens_spec(10, 5, 20, 10)
#' @export
sum_sens_spec <- function(tp, fp, tn, fn) {
    sesp <- sens_spec(tp, fp, tn, fn)
    sesp <- cbind(rowSums(sesp))
    colnames(sesp) <- "Sum_Sens_Spec"
    return(sesp)
}



#' Calculate absolute difference of sensitivity and specificity
#' Calculate the absolute difference of sensitivity and specificity
#' from the elements of a confusion matrix,
#' that is true positives, false positives, true negatives and false negatives.
#' The inputs must be vectors of equal length.
#' @param tp (numeric) number of true positives.
#' @param fp (numeric) number of false positives.
#' @param tn (numeric) number of true negatives.
#' @param fn (numeric) number of false negatives.
#' @examples
#' abs_d_sesp(10, 5, 20, 10)
#' @export
abs_d_sesp <- function(tp, fp, tn, fn) {
    sesp <- sens_spec(tp, fp, tn, fn)
    abs_d_sesp <- abs(sesp[, 1] - sesp[, 2])
    abs_d_sesp <- matrix(abs_d_sesp, ncol = 1)
    colnames(abs_d_sesp) <- "abs_d_sesp"
    return(abs_d_sesp)
}


#' Calculate Cohen's Kappa
#' Calculate the Kappa metric from the elements of a 2x2 confusion matrix,
#' that is true positives, false positives, true negatives and false negatives.
#' The inputs must be vectors of equal length.
#' @param tp (numeric) number of true positives.
#' @param fp (numeric) number of false positives.
#' @param tn (numeric) number of true negatives.
#' @param fn (numeric) number of false negatives.
#' @examples
#' cohens_kappa(10, 5, 20, 10)
#' @return A numeric matrix with the column name "Kappa".
#' @export
cohens_kappa <- function(tp, fp, tn, fn) {
    mrg_a <- ((tp + fn) * (tp + fp)) / (tp + fn + fp + tn)
    mrg_b <- ((fp + tn) * (fn + tn)) / (tp + fn + fp + tn)
    EA     <- (mrg_a + mrg_b) / (tp + fn + fp + tn)
    OA     <- (tp + tn) / (tp + fn + fp + tn)
    res <- matrix((OA - EA) / (1 - EA), ncol = 1)
    colnames(res) <- "Kappa"
    return(res)
}

#' Calculate the odds ratio
#' Calculate the (diagnostic) odds ratio from the elements of a confusion matrix,
#' that is true positives, false positives, true negatives and false negatives.
#' The odds ratio is defined as: (TP / FP) / (TN / FN)
#' The inputs must be vectors of equal length.
#' @param tp (numeric) number of true positives.
#' @param fp (numeric) number of false positives.
#' @param tn (numeric) number of true negatives.
#' @param fn (numeric) number of false negatives.
#' @examples
#' odds_ratio(10, 5, 20, 10)
#' @export
odds_ratio <- function(tp, fp, tn, fn) {
    or <- (tp / fp) / (fn / tn)
    or <- matrix(or, ncol = 1)
    colnames(or) <- "odds_ratio"
    return(or)
}
