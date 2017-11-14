#' Determine an optimal cutpoint for the Youden-Index assuming normal distributions
#'
#' An optimal cutpoint maximizing the Youden- or J-Index
#' (sensitivity + specificity - 1) is calculated parametrically assuming
#' normally distributed data.
#'
#' @param data A data frame or tibble in which the columns that are given in x
#' and class can be found.
#' @param x (character) The variable name to be used for classification,
#' e.g. predictions or test values.
#' @param class (character) The variable name indicating class membership.
#' @param pos_class The value of class that indicates the positive class.
#' @param neg_class The value of class that indicates the negative class.
#' @param direction (character) Use ">=" or "<=" to select whether an x value
#' >= or <= the cutoff predicts the positive class.
#' @param ... To capture further arguments that are always passed to the method
#' function by cutpointr. The cutpointr function passes data, x, class,
#' metric_func, direction, pos_class and neg_class to the method function.
#' @examples
#' data(suicide)
#' oc_youden_normal(suicide, "dsi", "suicide",
#'   pos_class = "yes", neg_class = "no", direction = ">=")
#' cutpointr(suicide, dsi, suicide, method = oc_youden_normal)
#' @export
oc_youden_normal <- function(data, x, class, pos_class = NULL, neg_class = NULL,
                             direction, ...) {
    stopifnot(is.character(x))
    stopifnot(is.character(class))
    iv <- unlist(data[, x])
    if (any_inf(iv)) stop("Only finite values allowed in oc_youden_normal")
    cla <- unlist(data[, class])
    if (direction %in% c(">", ">=")) {
        patients <- iv[cla == pos_class]
        controls <- iv[cla == neg_class]
    } else if (direction %in% c("<", "<=")) {
        patients <- iv[cla == neg_class]
        controls <- iv[cla == pos_class]
    }
    m_h <- mean(controls)
    sd_h <- stats::sd(controls)
    m_d <- mean(patients)
    sd_d <- stats::sd(patients)
    if (sd_h == sd_d) {
        c <- (m_h+m_d)/2
    } else if (any(sd_h == 0, sd_d == 0)) {
        # if sd_h = 0 and/or sd_d = 0 the cutoff would be NaN
        c <- (m_h+m_d)/2
    } else {
        c <- ((m_d*sd_h^2 - m_h*sd_d^2) - sd_h*sd_d*(sqrt((m_h-m_d)^2 + (sd_h^2-sd_d^2) * log(sd_h^2/sd_d^2)))) /
            (sd_h^2-sd_d^2)
    }

    # Extremely high or low cutoffs can result if m_d < m_h and direction = ">="
    if (c < min(c(controls, patients))) {
        warning(paste("Cutpoint", c, "was restricted to range of independent variable"))
        c <- min(c(controls, patients))
    } else if (c > max(c(controls, patients))) {
        warning(paste("Cutpoint", c, "was restricted to range of independent variable"))
        c <- max(c(controls, patients))
    }
    return(data.frame(optimal_cutpoint = c))
}

