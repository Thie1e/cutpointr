#' Find optimal cutpoint to maximize Kappa
# oc_kappa <- function(...) {
#     UseMethod("oc_kappa")
# }
#
# #' Find optimal cutpoint to maximize Kappa
# #' @export
# oc_kappa.default <- function(x, class,
#                              candidate_cuts = unique(x),
#                              pos_class = NULL, higher = NULL) {
#     #
#     # Preparation ---------
#     #
#     if (length(x) != length(class)) stop("The x and class vectors are of different length")
#     if (length(unique(class)) != 2) stop(paste("Expecting two classes, got", length(unique(class))))
#     if (is.null(pos_class)) {
#         pos_class <- unique(class)[1]
#         message(paste("Assuming", pos_class, "as positive class"))
#     }
#     neg_x <- x[class != pos_class]
#     pos_x <- x[class == pos_class]
#     if (is.null(higher)) {
#         if (mean(neg_x) < mean(pos_x)) {
#             message("Assuming the positive class has higher x values")
#             higher = T
#         } else {
#             message("Assuming the positive class has lower x values")
#             higher = F
#         }
#     }
#     if (higher) {
#         candidate_cuts <- unique(c(-Inf, candidate_cuts))
#     } else {
#         candidate_cuts <- unique(c(candidate_cuts, Inf))
#     }
#     #
#     # End preparation -------
#     #
#
#     if (higher) {
#         fh <- ecdf(neg_x)
#         gd <- ecdf(pos_x)
#         sens_c <- 1 - gd(candidate_cuts)
#         spec_c <- fh(candidate_cuts)
#         abs_d_sesp <- abs(sens_c - spec_c)
#         oc <- mean(candidate_cuts[abs_d_sesp == min(abs_d_sesp)])
#         abs_d_sensspec_oc <- abs(1 - gd(oc) - fh(oc))
#
#         # print(data.frame(candidate_cuts, sens_c, spec_c))
#
#         res <- data.frame(optimal_cutpoint = oc,
#                           abs_d_sensspec   = abs_d_sensspec_oc)
#     } else {
#         fh <- ecdf(-neg_x)
#         gd <- ecdf(-pos_x)
#         sens_c <- 1 - gd(-candidate_cuts)
#         spec_c <- fh(-candidate_cuts)
#         abs_d_sesp <- abs(sens_c - spec_c)
#         oc <- mean(-candidate_cuts[abs_d_sesp == min(abs_d_sesp)])
#         abs_d_sensspec_oc <- abs(1 - gd(oc) - fh(oc))
#         oc <- -oc
#
#         # print(data.frame(candidate_cuts, sens_c, spec_c))
#
#         res <- data.frame(optimal_cutpoint = oc,
#                           abs_d_sensspec   = abs_d_sensspec_oc)
#     }
#     return(res)
# }


kappa <- function(pred, obs, pos_class) {
    a <- sum(pred == pos_class & obs == pos_class) # TP
    d <- sum(pred != pos_class & obs != pos_class) # TN
    b <- sum(pred == pos_class & obs != pos_class) # FP
    c <- sum(pred != pos_class & obs == pos_class) # FN
    kappa_cf(a, b, c, d)
}
kappa_cf <- function(a, b, c, d) {
    marg_a <- ((a + b) * (a + c)) / (a + b + c + d)
    marg_b <- ((c + d) * (b + d)) / (a + b + c + d)
    EA     <- (marg_a + marg_b) / (a + b + c + d)
    OA     <- (a + d) / (a + b + c + d)
    (OA - EA) / (1 - EA)
}

# pred <- sample(0:1, size = 10, replace = T)
# obs <- sample(0:1, size = 10, replace = T)
# kappa(pred, obs, pos_class = 1)


oc_kappa_simple <- function(x, class,
                             candidate_cuts = unique(x),
                             pos_class = NULL, neg_class = NULL, higher = TRUE) {
    if (higher) {
        candidate_cuts <- unique(c(-Inf, candidate_cuts))
    } else {
        candidate_cuts <- unique(c(candidate_cuts, Inf))
    }
    #
    # End preparation -------
    #

    if (higher) {
        kappas <- sapply(candidate_cuts, function(cu) {
            pred <- ifelse(x > cu, pos_class, neg_class)
            obs <- class
            kappa(pred, obs, pos_class = pos_class)
        })
        oc_ind <- which(kappas == max(kappas))
        kappa_oc <- mean(kappas[oc_ind])
        oc <- mean(candidate_cuts[oc_ind])
        res <- data.frame(optimal_cutpoint = oc,
                          kappa            = kappa_oc)
    } else {
        kappas <- sapply(candidate_cuts, function(cu) {
            pred <- ifelse(x < cu, pos_class, neg_class)
            obs <- class
            kappa(pred, obs, pos_class = pos_class)
        })
        oc_ind <- which(kappas == max(kappas))
        kappa_oc <- mean(kappas[oc_ind])
        oc <- mean(candidate_cuts[oc_ind])
        res <- data.frame(optimal_cutpoint = oc,
                          kappa            = kappa_oc)
    }
    # print(kappas)
    return(res)
}


# tempx <- 1:7
# tempy <- c("a", "a", "a", "b", "b", "b", "b")
# cbind(tempx, tempy)
# optcut_emp_eqsensspec.default(tempx, tempy, higher = T, pos_class = "b")
# oc_kappa_simple(tempx, tempy, higher = T, pos_class = "b")
#
#
#
# tempx <- sort(rnorm(1000))
# tempy <- sample(c("a", "b"), size = 1000, replace = T)
# microbenchmark::microbenchmark(
#     optcut_emp_eqsensspec.default(tempx, tempy, higher = T, pos_class = "b"),
#     oc_kappa_simple(tempx, tempy, pos_class = "b"),
#     times = 50
# )
