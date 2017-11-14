#' Determine an optimal cutpoint maximizing the Youden-Index based on kernel smoothed densities
#'
#' Instead of searching for an optimal cutpoint to maximize (sensitivity +
#' specificity - 1) on the ROC curve, this function first smoothes the empirical
#' distributions of x per class. The smoothing is done using a binned kernel
#' density estimate. The bandwith is automatically selected using the direct
#' plug-in method.
#'
#' The functions for calculating the kernel density estimate and the bandwith
#' are both from the KernSmooth package with default parameters, except for
#' the bandwidth selection, which uses the standard deviation as scale estimate.
#'
#' @inheritParams oc_youden_normal
#' @source Fluss, R., Faraggi, D., & Reiser, B. (2005). Estimation of the
#' Youden Index and its associated cutoff point. Biometrical Journal, 47(4), 458–472.
#' @source   Matt Wand (2015). KernSmooth: Functions for Kernel Smoothing
#' Supporting Wand & Jones (1995). R package version 2.23-15.
#' https://CRAN.R-project.org/package=KernSmooth
#' @examples
#' data(suicide)
#' if (require(KernSmooth)) {
#'   oc_youden_kernel(suicide, "dsi", "suicide", oc_metric = "Youden",
#'   pos_class = "yes", neg_class = "no", direction = ">=")
#'   ## Within cutpointr
#'   cutpointr(suicide, dsi, suicide, method = oc_youden_kernel)
#' }
#' @export
oc_youden_kernel <- function(data, x, class, pos_class, neg_class,
                             direction, ...) {
    stopifnot(is.character(x))
    stopifnot(is.character(class))
    iv <- unlist(data[, x])
    cla <- unlist(data[, class])
    if (direction %in% c(">", ">=")) {
        x_pos <- iv[cla == pos_class]
        x_neg <- iv[cla == neg_class]
    } else {
        x_neg <- iv[cla == pos_class]
        x_pos <- iv[cla == neg_class]
    }

    bw_n <- KernSmooth::dpik(x_neg, "stdev")
    neg_k <- KernSmooth::bkde(x_neg, bandwidth = bw_n)
    bw_p <- KernSmooth::dpik(x_pos, "stdev")
    pos_k <- KernSmooth::bkde(x_pos, bandwidth = bw_p)


    oc <- stats::optimize(
        f = youden_kern,
        interval = c(min(c(x_pos, x_neg)), max(c(x_pos, x_neg))),
        maximum = TRUE,
        neg_k = neg_k,
        pos_k = pos_k
    )$maximum

    return(data.frame(optimal_cutpoint = oc))
}

youden_kern <- function(threshold = NULL, neg_k, pos_k) {
    youden <- vector_auc(neg_k$x, neg_k$y, to = threshold) -
        vector_auc(pos_k$x, pos_k$y, to = threshold)
    return(youden)
}


#' @source Forked from MESS
vector_auc <- function (x, y, from = min(x), to = max(x), ...) {
    if (length(x) != length(y))
        stop("x and y must have the same length")
    if (length(unique(x)) < 2)
        return(NA)
    if (to > max(x)) to <- max(x)
    values <- stats::approx(x, y, xout = sort(unique(c(from, to,
                                                x[x > from & x < to]))), ...)
    res <- 0.5 * sum(diff(values$x) * (values$y[-1] + values$y[-length(values$y)]))
    res
}


