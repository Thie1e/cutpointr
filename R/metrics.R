# #' Calculate AUC
# #' @source MESS::auc
# auc <- function (x, y, from = min(x), to = max(x))
# {
#     # MESS::auc with type = "linear" and absolutearea = F
#     if (length(x) != length(y))
#         stop("x and y must have the same length")
#     if (length(unique(x)) < 2)
#         return(NA)
#     values <- approx(x, y, xout = sort(unique(c(from, to, x[x > from & x < to]))), ...)
#     0.5 * sum(diff(values$x) * (values$y[-1] + values$y[-length(values$y)]))
# }

#' Calculate sensitivity
sens <- function(obs, preds, pos_class) {
    binary_obs <- obs == pos_class
    binary_preds <- preds == pos_class
    sum(binary_preds & binary_obs) / sum(binary_obs)
}

#' Calculate specificity
spec <- function(obs, preds, pos_class) {
    binary_obs <- obs == pos_class
    binary_preds <- preds == pos_class
    sum(!binary_preds & !binary_obs) / sum(!binary_obs)
}

sens_spec <- function(obs, preds, pos_class) {
    binary_obs <- obs == pos_class
    binary_preds <- preds == pos_class
    sens <- sum(binary_preds & binary_obs) / sum(binary_obs)
    spec <- sum(!binary_preds & !binary_obs) / sum(!binary_obs)
    c(Sensitivity = sens, Specificity = spec)
}


#' Calculate Youden Index (Sensitivity + Specificity - 1)
youden <- function(obs, preds, pos_class) {
    sens(obs = obs, preds = preds, pos_class = pos_class) +
        spec(obs = obs, preds = preds, pos_class = pos_class) -
        1
}
