conf_mat <- function(obs, preds, pos_class, neg_class) {
    binary_obs <- obs == pos_class
    binary_preds <- preds == pos_class
    tp <- sum(binary_preds & binary_obs)
    fp <- sum(binary_preds & (!binary_obs))
    binary_obs <- obs == neg_class
    binary_preds <- preds == neg_class
    tn <- sum(binary_preds & binary_obs)
    fn <- sum(binary_preds & (!binary_obs))
    c(TP = tp, FP = fp, TN = tn, FN = fn)
}

# conf_mat <- function(obs, preds, pos_class) {
#     cf <- table(obs == pos_class, preds == pos_class)
#     c(TP = cf[2, 2], FP = cf[1, 2], TN = cf[1, 1], FN = cf[2, 1])
# }

sens_spec <- function(tp, fp, tn, fn) {
    sens <- tp / (tp + fn)
    spec <- tn / (tn + fp)
    res <- cbind(sens, spec)
    colnames(res) <- c("Sensitivity", "Specificity")
    return(res)
}

sesp_from_oc <- function(x, class, oc, direction, pos_class, neg_class) {
    if (direction == ">=") {
        predictions <- ifel_pos_neg(x >= oc, pos_class, neg_class)
    } else if (direction == "<=") {
        predictions <- ifel_pos_neg(x <= oc, pos_class, neg_class)
    }
    cm <- conf_mat(obs = class, preds = predictions, pos_class, neg_class)
    sens_spec(tp = cm["TP"], fp = cm["FP"], tn = cm["TN"], fn = cm["FN"])
}

sesp_from_oc2 <- function(roc_curve, oc, direction) {
    if (direction == ">=") {
        opt_ind <- max(which(roc_curve$x.sorted >= oc))
    } else if (direction == "<=") {
        opt_ind <- min(which(roc_curve$x.sorted <= oc))
    }
    sens_spec(tp = roc_curve$tp[opt_ind], fp = roc_curve$fp[opt_ind],
              tn = roc_curve$tn[opt_ind], fn = roc_curve$fn[opt_ind])
}

#' @source Forked from the AUC package
auc <- function(tpr, fpr) {
    res <- 0
    for (i in 2:length(fpr)) {
        res <- res + 0.5 * abs(fpr[i] - fpr[i - 1]) * (tpr[i] + tpr[i - 1])
    }
    res
}


#' Calculate accuracy
#' Calculate accuracy from the elements of a confusion matrix, that is
#' true positives, false positives, true negatives and false negatives.
#' @param tp (numeric) number of true positives
#' @param fp (numeric) number of false positives
#' @param tn (numeric) number of true negatives
#' @param fn (numeric) number of false negatives
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
#' @param tp (numeric) number of true positives
#' @param fp (numeric) number of false positives
#' @param tn (numeric) number of true negatives
#' @param fn (numeric) number of false negatives
#' @examples
#' youden(10, 5, 20, 10)
#' @export
youden <- function(tp, fp, tn, fn) {
    sesp <- sens_spec(tp, fp, tn, fn)
    youden <- cbind(rowSums(sesp) - 1)
    colnames(youden) <- "Youden_Index"
    return(youden)
}


#' Calculate absolute difference of sensitivity and specificity
#' Calculate the absolute difference of sensitivity and specificity
#' from the elements of a confusion matrix,
#' that is true positives, false positives, true negatives and false negatives.
#' @param tp (numeric) number of true positives
#' @param fp (numeric) number of false positives
#' @param tn (numeric) number of true negatives
#' @param fn (numeric) number of false negatives
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


#' Calculate the Kappa metric
#' Calculate the Kappa metric from the elements of a 2x2 confusion matrix,
#' that is true positives, false positives, true negatives and false negatives.
#' @param tp (numeric) number of true positives
#' @param fp (numeric) number of false positives
#' @param tn (numeric) number of true negatives
#' @param fn (numeric) number of false negatives
#' @examples
#' kappa(10, 5, 20, 10)
#' @export
kappa <- function(tp, fp, tn, fn) {
    mrg_a <- ((tp + fn) * (tp + fp)) / (tp + fn + fp + tn)
    mrg_b <- ((fp + tn) * (fn + tn)) / (tp + fn + fp + tn)
    EA     <- (mrg_a + mrg_b) / (tp + fn + fp + tn)
    OA     <- (tp + tn) / (tp + fn + fp + tn)
    res <- matrix((OA - EA) / (1 - EA), ncol = 1)
    colnames(res) <- "Kappa"
    return(res)
}
