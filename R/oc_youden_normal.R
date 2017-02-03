#' Determine an optimal cutpoint for the Youden-Index assuming normal distributions
#'
#' @export
oc_youden_normal <- function(data, x, class, pos_class = NULL, neg_class = NULL,
                            direction, ...) {
    if (direction %in% c("<", "<=")) stop("Not yet implemented")
    stopifnot(is.character(x))
    stopifnot(is.character(class))
    iv <- unlist(data[, x])
    if (any(!is.finite(iv))) stop("Only finite values allowed in oc_youden_normal")
    cla <- unlist(data[, class])
    patients <- iv[cla == pos_class]
    controls <- iv[cla == neg_class]
    m_h <- mean(controls)
    sd_h <- sd(controls)
    m_d <- mean(patients)
    sd_d <- sd(patients)
    if (sd_h == sd_d) {
        c <- (m_h+m_d)/2
    } else if (any(sd_h == 0, sd_d == 0)) {
        # mit sd_h = 0 und/oder sd_d = 0 wird der cutoff sonst NaN
        c <- (m_h+m_d)/2
    } else {
        c <- ((m_d*sd_h^2 - m_h*sd_d^2) - sd_h*sd_d*(sqrt((m_h-m_d)^2 + (sd_h^2-sd_d^2) * log(sd_h^2/sd_d^2)))) /
            (sd_h^2-sd_d^2)
    }
    cut <- ceiling(c)
    # Die Formel kann extrem niedrige oder hohe Cutoffs ergeben, z.B. wenn m_d < m_h
    if (cut < min(c(controls, patients))) {
        cut <- min(c(controls, patients))
    } else if (cut > max(c(controls, patients))) {
        cut <- max(c(controls, patients))
    }
    return(data.frame(optimal_cutpoint = cut))
}
