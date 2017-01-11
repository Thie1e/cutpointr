#' Calculate a metric function over given possible cutpoints
#'
#' A metric function, e.g. a function to calculate accuracy, has to be supplied
#' that will be applied to classifications of the data at every possible cutpoint,
#' i.e. all values in candidate_cuts.
#'
#' @param data A data frame or tbl_df
#' @param x (character) The column name of the predictor variable
#' @param class (character) The column name of the class (outcome) variable
#' @param candidate_cuts (numeric vector) Optionally, the cutoffs to be supplied
#' to metric_func can be specified. By default all unique values in x will be used.
#' @param metric_func (function) A function that takes one or more of the following
#' arguments:
#' \itemize{
#'  \item preds
#'  \item obs
#'  \item pos_class
#'  \item ...
#' }
#' The function should return a single numeric value.
#' @param pos_class The label of the positive class. For example, if the class
#' column in data is numeric, this should be numeric, too.
#' @param neg_class (optional) The label of the negative class. If not given,
#' if will be assumed as the value in the class column that is not pos_class.
#' Specifying neg_class is faster for larger data.
#' @param direction (character) This should be ">" (default) or "<" to specify
#' whether x values larger or smaller than a cutoff indicate the positive class.
#' @param ... Further optional arguments that will be passed to metric_func
#' @return A data frame with columns "cutpoint" and "metric"
#' @export
gather_cutoffs <- function(data, x, class,  candidate_cuts = unique(data[, x]),
                           metric_func = NULL, pos_class, neg_class,
                           direction = ">", ...) {
    stopifnot(is.character(x))
    stopifnot(is.character(class))

    metric_name <- as.character(match.call()$metric_func)
    candidate_cuts <- sort(candidate_cuts)

    if (is.null(neg_class)) {
        neg_class <- unique(stats::na.omit(data[, class]))
        neg_class <- neg_class[neg_class != pos_class]
    }

    `%direc%` <- ifelse(direction == ">", `>`, `<`)

    metrics <- purrr::map_dbl(candidate_cuts, function(cutpoint) {
        p <- ifel_pos_neg(data[, x] %direc% cutpoint, pos_class, neg_class)
        metric_func(preds = p, obs = data[, class], pos_class, ...)
    })
    res <- data.frame(cutpoint = candidate_cuts, metric = metrics)
    return(res)
}

