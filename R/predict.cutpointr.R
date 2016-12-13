#' @export
predict.cutpointr <- function(cutpointr, newdata = NULL) {
    if (is.null(newdata)) {
        newdata <- cutpointr$data
    }
    outcome_name   <- cutpointr$outcome[1]
    predictor_name <- cutpointr$predictor[1]
    pos_class      <- cutpointr$pos_class[1]
    neg_class      <- cutpointr$neg_class[1]

    # Cutpoint by group
    if (any(colnames(cutpointr) == "group")) {
        grouping_name <- unique(cutpointr$grouping)
        if(!any(colnames(newdata) == grouping_name)) stop("grouping variable not found in newdata")
        optimal_cut <- cutpointr$optimal_cutpoint[cutpointr$group == newdata$group]
        opt_cut_ind <- purrr::map_int(newdata[, grouping_name], function(g) {
            which(opt_cut$group == g)
        })
        optimal_cuts <- cutpointr$optimal_cutpoint[opt_cut_ind]
        if (cutpointr$direction[1] == ">") {
            preds <- newdata[, predictor_name] > optimal_cuts
            preds <- ifelse(preds, pos_class, neg_class)
        } else if (cutpointr$direction[1] == "<") {
            preds <- newdata[, predictor_name] < optimal_cuts
            preds <- ifelse(preds, pos_class, neg_class)
        }
    } else {
        optimal_cut <- cutpointr$optimal_cutpoint
        if (cutpointr$direction[1] == ">") {
            preds <- newdata[, predictor_name] > optimal_cut
            preds <- ifelse(preds, pos_class, neg_class)
        } else if (cutpointr$direction[1] == "<") {
            preds <- newdata[, predictor_name] < optimal_cut
            preds <- ifelse(preds, pos_class, neg_class)
        }
    }
    return(preds)
}