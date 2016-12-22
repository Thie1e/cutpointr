#' Determine optimal cutpoint by maximizing the sum of sensitivity and specificity
#'
#' Cutpoints are taken from the sample values (with the optional addition of midpoints
#' between these values) and the classification quality of these cupoints is
#' evaluated based on the chosen method.
#'
#' @param data (data.frame) The dependent and independent variables
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
#' @examples
#' library(OptimalCutpoints)
#' data(elas)
#' optcut_emp_youden(elas$elas, elas$status, pos_class = 1)
optcut_emp_youden.default <- function(x, class,
                                      candidate_cuts = unique(x),
                                      pos_class = NULL, higher = NULL) {
    #
    # Preparation ---------
    #
    if (length(x) != length(class)) stop("The x and class vectors are of different length")
    if (length(unique(class)) != 2) stop(paste("Expecting two classes, got", length(unique(class))))
    if (is.null(pos_class)) {
        pos_class <- unique(class)[1]
        message(paste("Assuming", pos_class, "as positive class"))
    }
    neg_x <- x[class != pos_class]
    pos_x <- x[class == pos_class]
    if (is.null(higher)) {
        if (mean(neg_x) < mean(pos_x)) {
            message("Assuming the positive class has higher x values")
            higher = T
        } else {
            message("Assuming the positive class has lower x values")
            higher = F
        }
    }
    if (higher) {
        candidate_cuts <- unique(c(-Inf, candidate_cuts))
    } else {
        candidate_cuts <- unique(c(candidate_cuts, Inf))
    }
    #
    # End preparation -------
    #

    fh <- ecdf(neg_x)
    gd <- ecdf(pos_x)
    if (higher) {
        youden <- fh(candidate_cuts) - gd(candidate_cuts)
        oc <- mean(candidate_cuts[youden == max(youden)])
        youden_oc <- fh(oc) - gd(oc)
        res <- data.frame(optimal_cutpoint = oc,
                          youden           = youden_oc)
    } else {
        youden <- gd(candidate_cuts) - fh(candidate_cuts)
        oc <- mean(candidate_cuts[youden == max(youden)])
        youden_oc <- gd(oc) - fh(oc)
        res <- data.frame(optimal_cutpoint = oc,
                          youden           = youden_oc)
    }
    return(res)
}

# youden_rocr <- function(x,class){
#   pred<-ROCR::prediction(x,class)
#   perf<-ROCR::performance(pred, "sens", "spec")
#
#   cutpoint<-slot(perf, "alpha.values")[[1]]
#   spec<-slot(perf, "x.values")[[1]]
#   sens<-slot(perf, "y.values")[[1]]
#   res_tab<-data.frame(cutpoint, sens, spec)
#
#   oc <- res_tab[which.max(abs(res_tab$sens + res_tab$spec)),]
#
#   return(oc$cutpoint)
# }

# tempx <- rnorm(1000)
# tempy <- sample(c("a", "b"), size = 1000, replace = T)
# dat <- data.frame(value = tempx, class = tempy)
# microbenchmark::microbenchmark(
#     optcut_emp_youden.default(x = tempx, class = tempy, higher = F,
#                               pos_class = "a")
#     )
# microbenchmark::microbenchmark(
#     youden_rocr(x = tempx, class = tempy)
#     )
# microbenchmark::microbenchmark(
#     cutpointr(dat, value, class, pos_class = "a", higher = F)
#     )

# Check higher TRUE / FALSE
# tempx <- 1:7
# tempy <- c("a", "a", "a", "b", "b", "b", "b")
# cbind(tempx, tempy)
# optcut_emp_youden.default(tempx, tempy, pos_class = "a", higher = F)


#' @rdname optcut_emp_youden
#' @param formula (formula) Formula for splitting classes by some numeric vector, e.g. class ~ x.
#' @export
optcut_emp_youden.formula <- function(formula, data) {
    stop("Not yet implemented")
}

