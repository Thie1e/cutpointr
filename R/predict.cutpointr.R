#' Predict using a cutpointr object
#'
#' Predictions are made on the \code{data.frame} in \code{newdata}
#' using either the variable name or by applying the same transformation to
#' the data as in \code{cutpointr}. The class of the output will be identical to the class
#' of the predictor.
#'
#' @param object a cutpointr object.
#' @param newdata a data.frame with a column that contains the predictor
#' variable.
#' @param cutpoint_nr if multiple optimal cutpoints were found this parameter
#' defines which one should be used for predictions. Can be a vector if
#' different cutpoint numbers are desired for different subgroups.
#' @param ... further arguments.
#' @examples
#' oc <- cutpointr(suicide, dsi, suicide)
#' ## Return in-sample predictions
#' predict(oc, newdata = data.frame(dsi = oc$data[[1]]$dsi))
#' @export
predict.cutpointr <- function(object, newdata, cutpoint_nr = 1, ...) {
    if (!("data.frame" %in% class(newdata))) {
        stop("newdata should be a data.frame")
    }
    stopifnot(length(cutpoint_nr) == 1 | length(cutpoint_nr) == nrow(object))
    predictor_name <- object$predictor[1]
    # The predictor may have been altered using NSE
    indep_var <- eval(parse(text = predictor_name), newdata, parent.frame())
    pos_class <- object$pos_class[1]
    neg_class <- object$neg_class[1]

    if (any(colnames(object) == "subgroup")) {
        grouping_name     <- unique(object$grouping)
        grouping_var_new  <- eval(parse(text = grouping_name), newdata, parent.frame())
        opt_cut_ind <- purrr::map_int(grouping_var_new, function(g) {
            which(object$subgroup == g)
        })
        if (length(cutpoint_nr) == 1) {
            optimal_cuts <- purrr::map_dbl(object$optimal_cutpoint, function(x) {
                x[cutpoint_nr]
            })
        } else {
            optimal_cuts <- purrr::map2(.x = 1:nrow(object),
                                        .y = cutpoint_nr,
                                        .f = function(i, nr) {
                object$optimal_cutpoint[[i]][nr]
            })
            optimal_cuts <- unlist(optimal_cuts)
        }
        optimal_cuts <- optimal_cuts[opt_cut_ind]
        if (object$direction[1] == ">=") {
            preds <- indep_var >= optimal_cuts
            preds <- ifel_pos_neg(preds, pos_class, neg_class)
        } else if (object$direction[1] == "<=") {
            preds <- indep_var <= optimal_cuts
            preds <- ifel_pos_neg(preds, pos_class, neg_class)
        }
    } else {
        optimal_cut <- object$optimal_cutpoint[[1]][cutpoint_nr]
        if (object$direction[1] == ">=") {
            preds <- indep_var >= optimal_cut
            preds <- ifel_pos_neg(preds, pos_class, neg_class)
        } else if (object$direction[1] == "<=") {
            preds <- indep_var <= optimal_cut
            preds <- ifel_pos_neg(preds, pos_class, neg_class)
        }
    }
    return(preds)
}