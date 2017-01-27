# sens_spec <- function(obs, preds, pos_class) {
#     neg_class <- unique(obs)
#     neg_class <- neg_class[neg_class != pos_class]
#     stopifnot(length(neg_class) == 1)
#     binary_obs <- obs == pos_class
#     binary_preds <- preds == pos_class
#     sens <- sum(binary_preds & binary_obs) / sum(binary_obs)
#     binary_obs <- obs == neg_class
#     binary_preds <- preds == neg_class
#     spec <- sum(binary_preds & binary_obs) / sum(binary_obs)
#     c(Sensitivity = sens, Specificity = spec)
# }
#
#
# sens_spec <- function(obs, preds, pos_class) {
#     neg_class <- unique(obs)
#     neg_class <- neg_class[neg_class != pos_class]
#     stopifnot(length(neg_class) == 1)
#     binary_obs <- obs == pos_class
#     binary_preds <- preds == pos_class
#     tp <- sum(binary_preds & binary_obs)
#     fp <- sum(binary_preds & (!binary_obs))
#     binary_obs <- obs == neg_class
#     binary_preds <- preds == neg_class
#     tn <- sum(binary_preds & binary_obs)
#     fn <- sum(binary_preds & !binary_obs)
#     sens <- tp / (tp + fn)
#     spec <- tn / (tn + fp)
#     c(Sensitivity = sens, Specificity = spec)
# }

conf_mat <- function(obs, preds, pos_class) {
    neg_class <- unique(c(obs, preds))
    neg_class <- neg_class[neg_class != pos_class]
    if (length(neg_class) == 0) neg_class <- "neg"
    binary_obs <- obs == pos_class
    binary_preds <- preds == pos_class
    tp <- sum(binary_preds & binary_obs)
    fp <- sum(binary_preds & (!binary_obs))
    binary_obs <- obs == neg_class
    binary_preds <- preds == neg_class
    tn <- sum(binary_preds & binary_obs)
    fn <- sum(binary_preds & !binary_obs)
    c(TP = tp, FP = fp, TN = tn, FN = fn)
}

sens_spec <- function(tp, fp, tn, fn) {
    sens <- tp / (tp + fn)
    spec <- tn / (tn + fp)
    res <- cbind(sens, spec)
    colnames(res) <- c("Sensitivity", "Specificity")
    return(res)
}

sesp_from_oc <- function(x, class, oc, direction, pos_class, neg_class) {
    if (direction == ">=") {
        predictions <- ifel_pos_neg(x > oc, pos_class, neg_class)
    } else if (direction == "<=") {
        predictions <- ifel_pos_neg(x < oc, pos_class, neg_class)
    }
    cm <- conf_mat(obs = class, preds = predictions, pos_class)
    sens_spec(tp = cm["TP"], fp = cm["FP"], tn = cm["TN"], fn = cm["FN"])
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

#' Calculate Youden-Index
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

#' @source Forked from the Metrics package.
auc <- function(actual, predicted, pos_class) {
    r <- rank(predicted)
    n_pos <- sum(actual == pos_class)
    n_neg <- length(actual) - n_pos
    (sum(r[actual == pos_class]) - n_pos * (n_pos + 1)/2)/(n_pos * n_neg)
}

