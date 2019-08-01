#' Calculate bootstrap confidence intervals from a cutpointr object
#'
#' Given a \code{cutpointr} object this function calculates a bootstrap
#' confidence interval for a selected variable.
#' Missing values are removed before calculating the quantiles.
#' Values of the selected variable are returned for the percentiles alpha / 2
#' and 1 - alpha / 2. The metrics in the bootstrap data frames of
#' \code{cutpointr} are suffixed with \code{_b} and \code{_oob} to indicate
#' in-bag and out-of-bag, respectively. For example, to calculate quantiles
#' of the in-bag AUC \code{variable = AUC_b} should be set.
#'
#' @param x (character) The numeric independent (predictor) variable.
#' @param variable Variables to calculate CI for
#' @param alpha Alpha level. Quantiles of the bootstrapped values are returned
#' for (alpha / 2) and 1 - (alpha / 2).
#' @return A data frame with the columns quantile and value
#' @examples
#' \dontrun{
#' opt_cut <- cutpointr(suicide, dsi, suicide, gender,
#'   metric = youden, boot_runs = 1000)
#' boot_ci(opt_cut, optimal_cutpoint, 0.05)
#' boot_ci(opt_cut, acc_oob, 0.05)
#' boot_ci(opt_cut, cohens_kappa_oob, 0.05)
#' boot_ci(opt_cut, AUC_b, 0.05)
#' }
#' @export
#' @family main cutpointr functions
boot_ci <- function(x, variable, alpha = 0.05) {
    if (alpha < 0 | alpha > 1) stop("alpha should be between 0 and 1.")
    if (!inherits(x, "cutpointr")) {
        stop("Only objects of type cutpointr are supported")
    }
    if (!has_boot_results(x)) {
        stop("No bootstrap results found. Was boot_runs > 0 in cutpointr?")
    }

    if (!("subgroup" %in% colnames(x))) {
        variable_sym <- rlang::ensym(variable)
        variable_expr <- rlang::enexpr(variable_sym)
        variable <- rlang::eval_tidy(expr = variable_expr, data = x$boot[[1]])
        values <- stats::quantile(variable, probs = c(alpha / 2, 1 - alpha / 2),
                           na.rm = TRUE)
        res <- tibble::tibble(
            quantile = c(alpha / 2, 1 - alpha / 2),
            values = values
        )
        return(res)
    } else {
        res <- purrr::map2_dfr(.x = x$subgroup, .y = x$boot,
                               .f = function(s, b) {
            variable_sym <- rlang::ensym(variable)
            variable_expr <- rlang::enexpr(variable_sym)
            variable <- rlang::eval_tidy(expr = variable_expr, data = b)
            values <- stats::quantile(variable, probs = c(alpha / 2, 1 - alpha / 2),
                               na.rm = TRUE)
            tibble::tibble(
                subgroup = s,
                quantile = c(alpha / 2, 1 - alpha / 2),
                values = values
            )
        })
        return(res)
    }
}