#' Test for equivalence of a metric
#'
#' This function performs a significance test based on the bootstrap results
#' of cutpointr to test whether a chosen metric is equal between subgroups
#' or between two cutpointr objects. The test statistic is calculated as
#' the standardized difference of the metric between groups. If \code{x}
#' contains subgroups, the test is run on all possible pairings of subgroups.
#' An additional adjusted p-value is returned in that case.
#'
#' The variable name is looked up in the columns of the bootstrap results
#' where the suffixes _b and _oob indicate in-bag and out-of-bag estimates,
#' respectively (controlled via the \code{in_bag} argument).
#' Possible values are optimal_cutpoint, AUC,
#' acc, sensitivity, specificity, and the metric that was selected
#' in \code{cutpointr}. Note that there is no "out-of-bag optimal cutpoint", so
#' when selecting \code{variable = optimal_cutpoint} the test will be based on
#' the in-bag data.
#'
#' The test statistic is calculated as z = (t1 - t2) / sd(t1 - t2) where t1 and
#' t2 are the metric values on the full sample and sd(t1 - t2) is the standard
#' deviation of the differences of the metric values per bootstrap repetition.
#' The test is two-sided.
#'
#' If two cutpointr objects are compared and the numbers of bootstrap repetitions
#' differ, the smaller number will be used.
#'
#' @param x A cutpointr object with bootstrap results
#' @param y If x does not contain subgroups another cutpointr object
#' @param variable The variable for testing
#' @param in_bag Whether the in-bag or out-of-bag results should be used for testing
#' @param correction The type of correction for multiple testing. Possible
#' values are as in p.adjust.methods
#'
#' @return A data.frame (a tibble) with the columns test_var, p, d, sd_d, z
#' and in_bag. If a grouped cutpointr object was tested, the additional
#' columns subgroup1, subgroup2 and p_adj are returned.
#'
#' @examples
#' \dontrun{
#' library(cutpointr)
#' library(dplyr)
#' set.seed(734)
#' cp_f <- cutpointr(suicide %>% filter(gender == "female"), dsi, suicide,
#'   boot_runs = 1000, boot_stratify = TRUE)
#' set.seed(928)
#' cp_m <- cutpointr(suicide %>% filter(gender == "male"), dsi, suicide,
#'   boot_runs = 1000, boot_stratify = TRUE)
#' # No significant differences:
#' boot_test(cp_f, cp_m, AUC, in_bag = TRUE)
#' boot_test(cp_f, cp_m, sum_sens_spec, in_bag = FALSE)
#'
#' set.seed(135)
#' cp <- cutpointr(suicide, dsi, suicide, gender, boot_runs = 1000,
#'   boot_stratify = TRUE)
#' # Roughly same result as above:
#' boot_test(cp, variable = AUC, in_bag = TRUE)
#' boot_test(cp, variable = sum_sens_spec, in_bag = FALSE)
#' }
#'
#' @export
#' @family main cutpointr functions
#' @source Robin, X., Turck, N., Hainard, A., Tiberti, N., Lisacek, F.,
#' Sanchez, J.-C., & Müller, M. (2011). pROC: An open-source package for R
#' and S+ to analyze and compare ROC curves. BMC Bioinformatics, 12(1), 77.
#' https://doi.org/10.1186/1471-2105-12-77
boot_test <- function(x, y = NULL, variable = "AUC", in_bag = TRUE,
                      correction = "holm") {
    if (!has_boot_results(x)) {
        stop(paste("x does not contain bootstrap results.",
                   "Was boot_runs > 0 in cutpointr?"))
    }
    stopifnot(inherits(x, "cutpointr"))
    if (!is.null(y)) {
        if (!has_boot_results(y)) {
            stop(paste("y does not contain bootstrap results.",
                       "Was boot_runs > 0 in cutpointr?"))
        }
        stopifnot(inherits(y, "cutpointr"))
    }
    # No boot_test if multiple cutpoints
    multi_cp_x <- purrr::map_lgl(x$boot, function(b) is.list(b$optimal_cutpoint))
    if (!is.null(y)) {
        multi_cp_y <- purrr::map_lgl(y$boot, function(b) is.list(b$optimal_cutpoint))
    } else {
        multi_cp_y <- FALSE
    }
    if (any(multi_cp_x) | any(multi_cp_y)) {
        stop(paste("boot_test does not support multiple optimal cutpoints.",
                   "See ?boot_test. Try setting break_ties = median",
                   "in cutpointr."))
    }
    variable <- rlang::enquo(variable)
    variable <- rlang::as_name(variable)
    # If y is also supplied, x and y should be ungrouped
    if ((!is.null(y)) & nrow(x) > 1) {
        stop(paste("When testing two cutpointr objects these objects must not",
                   "contain subgroups."))
    }
    if (in_bag) suffix <- "_b" else suffix <- "_oob"
    if (variable == "optimal_cutpoint") suffix <- ""
    if (is.null(y)) {
        # Test subgroups vs. each other
        combs <- utils::combn(x = x$subgroup, m = 2, simplify = FALSE)
        combs <- do.call(rbind, combs)
        combs <- data.frame(var1 = combs[, 1], var2 = combs[, 2],
                            stringsAsFactors = FALSE)
        test_res <- purrr::map2_dfr(.x = combs$var1, .y = combs$var2,
                                    .f = function(var1, var2) {
            dat_var1 <- x %>%
                dplyr::filter(.data$subgroup == var1) %>%
                dplyr::select(.data$boot) %>%
                tidyr::unnest(.data$boot) %>%
                dplyr::select(paste0(variable, suffix)) %>%
                unlist
            dat_var2 <- x %>%
                dplyr::filter(.data$subgroup == var2) %>%
                dplyr::select(.data$boot) %>%
                tidyr::unnest(.data$boot) %>%
                dplyr::select(paste0(variable, suffix)) %>%
                unlist
            na_v1 <- sum(is.na(dat_var1))
            na_v2 <- sum(is.na(dat_var2))
            if (na_v1 > 0 | na_v2 > 0) {
                message(var1, " vs. ", var2, ": ",
                        "Omitting missing values. Using ",
                        nrow(x$boot[[1]]) - max(na_v1, na_v2),
                        " observations.")
            }
            sdt <- stats::sd(dat_var1 - dat_var2, na.rm = TRUE)
            t1 <- x %>%
                dplyr::filter(.data$subgroup == var1) %>%
                dplyr::select(variable) %>%
                unlist
            t2 <- x %>%
                dplyr::filter(.data$subgroup == var2) %>%
                dplyr::select(variable) %>%
                unlist
            z <- (t1 - t2) / sdt
            if (t1 - t2 == 0 & is.nan(sdt)) z  <- 0
            p <- stats::pnorm(-abs(z)) * 2
            tibble::tibble(subgroup1 = var1,
                           subgroup2 = var2,
                           test_var = variable,
                           p = p,
                           p_adj = NA,
                           d = t1 - t2,
                           sd_d = sdt,
                           z = z,
                           in_bag = in_bag)
        })
        test_res$p_adj <- stats::p.adjust(p = test_res$p, method = correction)
    } else if (!(is.null(y))) {
        # Test two ungrouped cutpointr objects vs. each other
        if (!has_boot_results(y)) {
            stop(paste("y does not contain bootstrap results.",
                       "Was boot_runs > 0 in cutpointr?"))
        }
        if (nrow(y) > 1) {
            stop(paste("When testing two cutpointr objects these objects must not",
                       "contain subgroups."))
        }
        dat_var1 <- x %>%
            dplyr::select(.data$boot) %>%
            tidyr::unnest(.data$boot) %>%
            dplyr::select(paste0(variable, suffix)) %>%
            unlist
        dat_var2 <- y %>%
            dplyr::select(.data$boot) %>%
            tidyr::unnest(.data$boot) %>%
            dplyr::select(paste0(variable, suffix)) %>%
            unlist
        if (length(dat_var1) != length(dat_var2)) {
            warning(paste("Unequal number of boot_runs. Using the lower",
                          "number of", min(length(dat_var1), length(dat_var2)),
                          "bootstrap repeats."))
            lower_runs <- min(length(dat_var1), length(dat_var2))
            dat_var1 <- dat_var1[1:lower_runs]
            dat_var2 <- dat_var2[1:lower_runs]
        }
        na_v1 <- sum(is.na(dat_var1))
        na_v2 <- sum(is.na(dat_var2))
        if (na_v1 > 0 | na_v2 > 0) {
            message("Omitting missing values. Using ",
                    nrow(x$boot[[1]]) - max(na_v1, na_v2),
                    " observations.")
        }
        sdt <- stats::sd(dat_var1 - dat_var2, na.rm = TRUE)
        t1 <- x %>%
            dplyr::select(variable) %>%
            unlist
        t2 <- y %>%
            dplyr::select(variable) %>%
            unlist
        z <- (t1 - t2) / sdt
        if (t1 - t2 == 0 & sdt == 0) z  <- 0
        p <- stats::pnorm(-abs(z)) * 2
        test_res <- tibble::tibble(test_var = variable,
                                   p = p,
                                   d = t1 - t2,
                                   sd_d = sdt,
                                   z = z,
                                   in_bag = in_bag)
    }
    class(test_res) <- c("boot_test", class(test_res))
    return(test_res)
}
