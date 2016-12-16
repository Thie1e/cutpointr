#' @export
predict.cutpointr <- function(cutpointr, newdata) {
    predictor_name <- cutpointr$predictor[1]
    # The predictor may have been altered using NSE
    indep_var <- eval(parse(text = predictor_name), newdata, parent.frame())
    pos_class <- cutpointr$pos_class[1]
    neg_class <- cutpointr$neg_class[1]

    if (any(colnames(cutpointr) == "group")) {
        grouping_name     <- unique(cutpointr$grouping)
        grouping_var_new  <- eval(parse(text = grouping_name), newdata, parent.frame())
        opt_cut_ind <- purrr::map_int(grouping_var_new, function(g) {
            which(cutpointr$group == g)
        })
        optimal_cuts <- cutpointr$optimal_cutpoint[opt_cut_ind]

        #-------
        # if(!any(colnames(newdata) == grouping_name)) stop("grouping variable not found in newdata")
        # optimal_cut <- cutpointr$optimal_cutpoint[cutpointr$group == newdata$group]
        # opt_cut_ind <- purrr::map_int(newdata[, grouping_name], function(g) {
        #     which(cutpointr$group == g)
        # })
        # optimal_cuts <- cutpointr$optimal_cutpoint[opt_cut_ind]
        #----------
        if (cutpointr$direction[1] == ">") {
            # preds <- newdata[, predictor_name] > optimal_cuts
            preds <- indep_var > optimal_cuts
            preds <- ifelse(preds, pos_class, neg_class)
        } else if (cutpointr$direction[1] == "<") {
            preds <- indep_var < optimal_cuts
            preds <- ifelse(preds, pos_class, neg_class)
        }
    } else {
        optimal_cut <- cutpointr$optimal_cutpoint
        if (cutpointr$direction[1] == ">") {
            # preds <- newdata[, predictor_name] > optimal_cut
            preds <- indep_var > optimal_cut
            preds <- ifelse(preds, pos_class, neg_class)
        } else if (cutpointr$direction[1] == "<") {
            # preds <- newdata[, predictor_name] < optimal_cut
            preds <- indep_var < optimal_cut
            preds <- ifelse(preds, pos_class, neg_class)
        }
    }
    return(preds)
}