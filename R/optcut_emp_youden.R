#' Determine optimal cutpoint by maximizing the sum of sensitivity and specificity
#'
#' Cutpoints are taken from the sample values (with the optional addition of midpoints
#' between these values) and the classification quality of these cupoints is
#' evaluated based on the chosen method.
#'
#' @param data (data.frame) The dependent and independent variables
#' @param group (numeric) An additional covariate that identifies subgroups. Separate
#' optimal cutpoints will be calculated for every group.
#' @export
#' @examples
#' dat <- iris[1:100, ]
#' optcut_emp_youden(dat$Sepal.Width, class = dat$Species)
optcut_emp_youden <- function(...){
    UseMethod("optcut_emp_youden")
}

#' @rdname optcut_emp_youden
#' @param x (numeric) The values that will be used for splitting
#' @param class (character / factor / numeric) The dependent binary class variable
#' @export
optcut_emp_youden.default <- function(x, class,
                                      candidate_cuts = unique(x),
                                      pos_class = NULL, higher = NULL) {
    #
    # Preparation ---------
    #
    if (length(x) != length(class)) stop("The x and class vectors are of different length")
    if (!is.null(higher) && higher == F) stop("higher = F not yet implemented")
    if (!is.factor(class)) class <- as.factor(class)
    if (length(unique(class)) != 2) stop(paste("Expecting two classes, got", length(unique(class))))
    if (is.null(pos_class)) {
        pos_class <- levels(class)[1]
        message(paste("Assuming", pos_class, "as positive class"))
    }
    # args <- as.list(match.call()[-1])
    # if (!("higher" %in% names(args))) {
    #     if (args$higher) {
    #         message(paste("Assuming higher x values imply positive class"))
    #     }
    # }
    neg_x <- x[class != pos_class]
    pos_x <- x[class == pos_class]
    if (is.null(higher)) {
        if (mean(neg_x) < mean(pos_x)) {
            message("Assuming the positive class has higher x values")
            higher = T
        } else {
            message("Assuming the positive class has lower x values")
            stop("higher = F not yet implemented")
            higher = F
        }
    }
    #
    # End preparation -------
    #
    fh <- ecdf(neg_x)
    gd <- ecdf(pos_x)
    youden <- fh(candidate_cuts - 1)-gd(candidate_cuts - 1)
    oc <- mean(candidate_cuts[youden == max(youden)])
    youden_oc <- fh(oc - 1) - gd(oc - 1)
    data.frame(optimal_cutpoint = oc,
               youden           = youden_oc)
}

#' @rdname optcut_emp_youden
#' @param formula (formula) Formula for splitting classes by some numeric vector, e.g. class ~ x.
#' @export
optcut_emp_youden.formula <- function(formula, group, data) {
    stop("Not yet implemented")
}


gather_cutoffs <- function(x, class,  candidate_cuts = unique(x),
                           metric_func = NULL, pos_class = NULL, higher = NULL) {
    #
    # Preparation ---------
    #
    if (!is.null(higher) && higher == F) stop("higher = F not yet implemented")
    if (!is.factor(class)) class <- as.factor(class)
    if (length(levels(class)) != 2) {
        stop(paste("Expecting two classes, got", length(levels(class))))
    }
    if (is.null(pos_class)) {
        pos_class <- levels(class)[1]
        message(paste("Assuming", pos_class, "as positive class"))
    }
    stopifnot(pos_class %in% levels(class))
    # args <- as.list(match.call()[-1])
    # if (!("higher" %in% names(args))) {
    #     if (args$higher) {
    #         message(paste("Assuming higher x values imply positive class"))
    #     }
    # }
    neg_x <- x[class != pos_class]
    pos_x <- x[class == pos_class]
    if (is.null(higher)) {
        if (mean(neg_x) < mean(pos_x)) {
            message("Assuming the positive class has higher x values")
            higher = T
        } else {
            message("Assuming the positive class has lower x values")
            stop("higher = F not yet implemented")
            higher = F
        }
    }
    #
    # End preparation -------
    #
    metric_name <- as.character(match.call()$metric_func)

    # Assuming higher = T
    # p <- ifelse(x > candidate_cuts, pos_class, 0)
    metrics <- purrr::map_dbl(candidate_cuts, function(cutpoint) {
        p <- ifelse(cutpoint > x, pos_class, 0)
        metric_func(preds = p, obs = class, pos_class)
    })
    res <- data.frame(cutpoint = candidate_cuts, metrics)
    colnames(res)[2] <- metric_name
    # class(res) <- c("cp_result", "data.frame") # Klasse cp_result wird in cutpointr gesetzt
    return(res)
}

# dat <- data.frame(value = rnorm(1000),
#                   class = sample(c("a", "b"), size = 1000, replace = T))
# gather_cutoffs(dat$value, class = dat$class, pos_class = "a", metric_func = youden, higher = T)
# microbenchmark::microbenchmark(
#     gather_cutoffs(dat$value, class = dat$class, pos_class = "a", metric_func = youden, higher = T)
# )
#
# microbenchmark::microbenchmark(
#     optcut_emp_youden.default(dat$value, dat$class)
# )
