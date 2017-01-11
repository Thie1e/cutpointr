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
    neg_class <- unique(obs)
    neg_class <- neg_class[neg_class != pos_class]
    stopifnot(length(neg_class) == 1)
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
    res <- c(sens, spec)
    names(res) <- c("Sensitivity", "Specificity")
    return(res)
}

sesp_from_oc <- function(x, class, oc, direction, pos_class, neg_class) {
    if (direction == ">") {
        predictions <- ifelse(x > oc, pos_class, neg_class)
    } else if (direction == "<") {
        predictions <- ifelse(x < oc, pos_class, neg_class)
    }
    cm <- conf_mat(obs = class, preds = predictions, pos_class)
    sens_spec(tp = cm["TP"], fp = cm["FP"], tn = cm["TN"], fn = cm["FN"])
}

accuracy <- function(tp, fp, tn, fn) {
    Accuracy = (tp + tn) / (tp + fp + tn + fn)
    names(Accuracy) <- "Accuracy"
    return(Accuracy)
}

