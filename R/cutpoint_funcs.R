
# Aus youden_estimation_bootOnly_v2
#' @export
youden_emp<-function(controls,patients){
    fh<-ecdf(controls)
    gd<-ecdf(patients)
    # cut<-unique(c(controls,patients)) ### Das hier sorgte für einen Bias
    cut <- 1:300
    youden<-fh(cut - 1)-gd(cut - 1)
    return(mean(cut[youden==max(youden)]))
}

youden_boot <- function(controls, patients, b = 200) {
    cutoffs_b <- sapply(1:b, function(b) {
        # Draw bootstrap sample
        cont_b <- sample(controls, replace = T)
        pat_b <- sample(patients, replace = T)
        youden_emp(cont_b, pat_b)
    })
    return(round(mean(cutoffs_b)))
}


# Aus youden_estimation_normalBootOnly_v2
#' @export
youden_normal<-function(controls, patients){
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
    return(cut)
}

#' @export
youden_normal_boot <- function(controls, patients, b = 200) {
    cutoffs_b <- sapply(1:b, function(b) {
        # Draw bootstrap sample
        cont_b <- sample(controls, replace = T)
        pat_b <- sample(patients, replace = T)
        youden_normal(cont_b, pat_b)
    })
    return(round(mean(cutoffs_b)))
}


# Aus youden_estimation_loessOnly_v1
#' @export
youden_loess<-function(controls, patients){
    fh<-ecdf(controls)
    gd<-ecdf(patients)
    # cut<-sort(unique(c(controls, patients)))
    cut <- min(c(controls, patients)):max(c(controls, patients))
    youden<-fh(cut - 1)-gd(cut - 1)

    modAs <- fANCOVA::loess.as(y = youden, x = cut, criterion = "aicc") # automatischer smoothing Parameter wie bei Leeflang et al.
    return((mean(modAs$x[modAs$fitted == max(modAs$fitted)]))) # modAs
}



# Aus youden_estimation_kernelOnly_v1
# Nonparametric Kernel
#' @export
youden_kernel <- function(cont, pat) {
    bw_c<-KernSmooth::dpik(cont, "minim")
    cont_k<-KernSmooth::bkde(cont, bandwidth=bw_c)
    bw_p<-KernSmooth::dpik(pat, "minim")
    pat_k<-KernSmooth::bkde(pat, bandwidth=bw_p)
    # candidateCuts <- sort(unique(c(cont, pat)))
    candidateCuts <- seq(from = min(c(cont, pat)), to = max(c(cont, pat)), by = 1)
    youden <- sapply(candidateCuts, function(thresh) {
        # Wenn thresh außerhalb des Intervalls liegt, ergibt auc() NA,
        # außer wenn solche Fälle per rule anders geregelt sind
        # Kann besonders in "einfachen" Szenarien wie 140/5 vorkommen
        auc_cont <- auc(cont_k$x,cont_k$y, to=thresh, rule = 2)
        auc_pat <- auc(pat_k$x,pat_k$y, to=thresh, rule = 2)
        youden <- auc_cont - auc_pat
        return(youden)
    })
    return(mean(candidateCuts[which(youden == max(youden, na.rm = T))]))
}


# Aus youden_estimation_kernelBootOnly_v1
# Nonparametric Kernel
#' @export
youden_kernel_boot_cutoff <- function(cont, pat) {
    # Durch die Bootstrap Samples gibt es manchmal den Fehler scale estimate is zero for input data,
    # wenn fast nur Duplikate gezogen werden
    bw_c<-try(KernSmooth::dpik(cont, "minim"), silent = T)
    class_bw_c <- class(bw_c)
    bw_p<-try(KernSmooth::dpik(pat, "minim"), silent = T)
    class_bw_p <- class(bw_p)
    if (class_bw_c == "try-error" & class_bw_p != "try-error") bw_c <- bw_p
    if (class_bw_p == "try-error" & class_bw_c != "try-error") bw_p <- bw_c
    if (class_bw_p == "try-error" & class_bw_c == "try-error") bw_p = bw_c = 1
    cont_k<-KernSmooth::bkde(cont, bandwidth=bw_c)
    pat_k<-KernSmooth::bkde(pat, bandwidth=bw_p)
    candidateCuts <- sort(unique(c(cont, pat)))
    # candidateCuts <- seq(from = min(c(cont, pat)), to = max(c(cont, pat)), by = 1)
    youden <- sapply(candidateCuts, function(thresh) {
        youden <- auc(cont_k$x,cont_k$y, to=thresh) - auc(pat_k$x,pat_k$y, to=thresh)
        return(youden)
    })
    return(mean(candidateCuts[which(youden == max(youden, na.rm = T))]))
}

#' @export
youden_kernel_boot <- function(controls, patients, b = 200) {
    cutoffs_b <- sapply(1:b, function(b) {
        # Draw bootstrap sample
        cont_b <- controls[sample(1:length(controls), replace = T)]
        pat_b <- patients[sample(1:length(patients), replace = T)]
        youden_kernel_boot_cutoff(cont_b, pat_b)
    })
    return(round(mean(cutoffs_b)))
}



