#' Predict using a cutpointr object
#'
#' Predictions are made on the data.frame in newdata. That data.frame should
#' contain a column with a name that is equal to the predictor name in
#' the cutpointr object. The class of the output is identical to the class
#' of the predictor.
#'
#' @param object a cutpointr object.
#' @param newdata a data.frame with a column that contains the predictor
#' variable
#' @param ... further arguments.
#' @examples
#' oc <- cutpointr(suicide, dsi, suicide)
#' ## Return in-sample predictions
#' predict(oc, newdata = data.frame(dsi = oc$data[[1]]$dsi))
#' @export
predict.cutpointr <- function(object, newdata, ...) {
    if (!("data.frame" %in% class(newdata))) {
        stop("newdata should be a data.frame with a correctly named predictor column")
    }
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
        optimal_cuts <- object$optimal_cutpoint[opt_cut_ind]
        if (object$direction[1] == ">=") {
            preds <- indep_var >= optimal_cuts
            preds <- ifel_pos_neg(preds, pos_class, neg_class)
        } else if (object$direction[1] == "<=") {
            preds <- indep_var <= optimal_cuts
            preds <- ifel_pos_neg(preds, pos_class, neg_class)
        }
    } else {
        optimal_cut <- object$optimal_cutpoint
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