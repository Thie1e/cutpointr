
#' Find optimal cutpoint by minimizing the difference between sensitivity and specificity
#' @export
optcut_emp_eqsensspec <- function(...) {
    UseMethod("optcut_emp_eqsensspec")
}

#' Find optimal cutpoint by minimizing the difference between sensitivity and specificity
#' @export
optcut_emp_eqsensspec.default <- function(x, class,
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

    if (higher) {
        fh <- ecdf(neg_x)
        gd <- ecdf(pos_x)
        sens_c <- 1 - gd(candidate_cuts)
        spec_c <- fh(candidate_cuts)
        abs_d_sesp <- abs(sens_c - spec_c)
        oc <- mean(candidate_cuts[abs_d_sesp == min(abs_d_sesp)])
        abs_d_sensspec_oc <- abs(1 - gd(oc) - fh(oc))

        # print(data.frame(candidate_cuts, sens_c, spec_c))

        res <- data.frame(optimal_cutpoint = oc,
                          abs_d_sensspec   = abs_d_sensspec_oc)
    } else {
        fh <- ecdf(-neg_x)
        gd <- ecdf(-pos_x)
        sens_c <- 1 - gd(-candidate_cuts)
        spec_c <- fh(-candidate_cuts)
        abs_d_sesp <- abs(sens_c - spec_c)
        oc <- mean(-candidate_cuts[abs_d_sesp == min(abs_d_sesp)])
        abs_d_sensspec_oc <- abs(1 - gd(oc) - fh(oc))
        oc <- -oc

        # print(data.frame(candidate_cuts, sens_c, spec_c))

        res <- data.frame(optimal_cutpoint = oc,
                          abs_d_sensspec   = abs_d_sensspec_oc)
    }
    return(res)
}

# equal_sen_spec<-function(x,class, neg_class, pos_class){
#   pred<-ROCR::prediction(x,class, label.ordering = c(neg_class, pos_class))
#   perf<-ROCR::performance(pred, "sens", "spec")
#
#   cutpoint<-slot(perf, "alpha.values")[[1]]
#   spec<-slot(perf, "x.values")[[1]]
#   sens<-slot(perf, "y.values")[[1]]
#   res_tab<-data.frame(cutpoint, sens, spec)
#   # print(res_tab)
#
#   oc <- res_tab[which.min(abs(res_tab$sens - res_tab$spec)),]
#
#   return(oc)
# }


# oc_sesp_simple <- function(x, class, pos_class, higher) {
#     candidate_cuts <- unique(x)
#     if (higher) {
#         candidate_cuts <- unique(c(-Inf, candidate_cuts))
#     } else {
#         candidate_cuts <- unique(c(candidate_cuts, Inf))
#     }
#
#     abs_d_sensspec <- lapply(candidate_cuts, function(c) {
#         if (higher) {
#             TP <- sum((x > c) & class == pos_class)
#             TN <- sum((x <= c) & class != pos_class)
#             sens <- TP / sum(class == pos_class)
#             spec <- TN / sum(class != pos_class)
#             data.frame(d_sesp = abs(sens - spec), TP = TP, TN = TN,
#                        sens = sens, spec = spec)
#         } else {
#             TP <- sum((x < c) & class == pos_class)
#             TN <- sum((x >= c) & class != pos_class)
#             sens <- TP / sum(class == pos_class)
#             spec <- TN / sum(class != pos_class)
#             data.frame(d_sesp = abs(sens - spec), TP = TP, TN = TN,
#                        sens = sens, spec = spec)
#         }
#     })
#     abs_d_sensspec <- do.call(rbind, abs_d_sensspec)
#
#     # print(data.frame(c = candidate_cuts,
#     #                  abs_d_sensspec))
#
#     oc_ind <- which(abs_d_sensspec$d_sesp == min(abs_d_sensspec$d_sesp))
#     if (length(oc_ind) > 1) warning("multiple optimal cuts found")
#     data.frame(oc = mean(candidate_cuts[oc_ind]),
#                d_sesp = min(abs_d_sensspec$d_sesp))
# }


# Check higher TRUE / FALSE
# tempx <- 1:7
# tempy <- c("a", "a", "a", "b", "b", "b", "b")
# cbind(tempx, tempy)
# equal_sen_spec(tempx, tempy, neg_class = "a", "b") # ROCR benutzt implizit >=
# optcut_emp_eqsensspec.default(tempx, tempy, higher = T, pos_class = "a")
# oc_sesp_simple(tempx, tempy, higher = T, pos_class = "a")
#
#
# tempx <- rnorm(1000)
# tempy <- sample(c("a", "b"), size = 1000, replace = T)
# microbenchmark::microbenchmark(
#     optcut_emp_youden.default(tempx, tempy, pos_class = "b", higher = T),
#     equal_sen_spec(tempx, tempy, "a", "b"), # ROCR benutzt implizit >=
#     optcut_emp_eqsensspec.default(tempx, tempy, higher = T, pos_class = "b"),
#     oc_sesp_simple(tempx, tempy, higher = T, pos_class = "b")
# )


