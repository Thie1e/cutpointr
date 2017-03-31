#' @export
predict.cutpointr <- function(object, newdata, ...) {
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
            # preds <- newdata[, predictor_name] > optimal_cuts
            preds <- indep_var >= optimal_cuts
            preds <- ifel_pos_neg(preds, pos_class, neg_class)
        } else if (object$direction[1] == "<=") {
            preds <- indep_var <= optimal_cuts
            preds <- ifel_pos_neg(preds, pos_class, neg_class)
        }
    } else {
        optimal_cut <- object$optimal_cutpoint
        if (object$direction[1] == ">=") {
            # preds <- newdata[, predictor_name] > optimal_cut
            preds <- indep_var >= optimal_cut
            preds <- ifel_pos_neg(preds, pos_class, neg_class)
        } else if (object$direction[1] == "<=") {
            # preds <- newdata[, predictor_name] < optimal_cut
            preds <- indep_var <= optimal_cut
            preds <- ifel_pos_neg(preds, pos_class, neg_class)
        }
    }
    return(preds)
}