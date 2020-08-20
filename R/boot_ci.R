#' Calculate bootstrap confidence intervals from a cutpointr object
#'
#' Given a \code{cutpointr} object that includes bootstrap results
#' this function calculates a bootstrap
#' confidence interval for a selected variable.
#' Missing values are removed before calculating the quantiles. In the case
#' of multiple optimal cutpoints all cutpoints / metric values are included
#' in the calculation.
#' Values of the selected variable are returned for the percentiles alpha / 2
#' and 1 - alpha / 2. The metrics in the bootstrap data frames of
#' \code{cutpointr} are suffixed with \code{_b} and \code{_oob} to indicate
#' in-bag and out-of-bag, respectively. For example, to calculate quantiles
#' of the in-bag AUC \code{variable = AUC_b} should be set.
#'
#' @param x (character) The numeric independent (predictor) variable.
#' @param variable Variable to calculate CI for
#' @param alpha Alpha level. Quantiles of the bootstrapped values are returned
#' for (alpha / 2) and 1 - (alpha / 2).
#' @param in_bag Whether the in-bag or out-of-bag results should be used for testing
#' @return A data frame with the columns quantile and value
#' @examples
#' \dontrun{
#' opt_cut <- cutpointr(suicide, dsi, suicide, gender,
#'   metric = youden, boot_runs = 1000)
#' boot_ci(opt_cut, optimal_cutpoint, in_bag = FALSE, alpha = 0.05)
#' boot_ci(opt_cut, acc, in_bag = FALSE, alpha = 0.05)
#' boot_ci(opt_cut, cohens_kappa, in_bag = FALSE, alpha = 0.05)
#' boot_ci(opt_cut, AUC, in_bag = TRUE, alpha = 0.05)
#' }
#' @export
#' @family main cutpointr functions
boot_ci <- function(x, variable, in_bag = TRUE, alpha = 0.05) {
    if (alpha < 0 | alpha > 1) stop("alpha should be between 0 and 1.")
    if (!inherits(x, "cutpointr")) {
        stop("Only objects of type cutpointr are supported")
    }
    if (!has_boot_results(x)) {
        stop("No bootstrap results found. Was boot_runs > 0 in cutpointr?")
    }

    if (!("subgroup" %in% colnames(x))) {
        variable <- rlang::enquo(variable)
        variable <- rlang::as_name(variable)
        if (in_bag) suffix <- "_b" else suffix <- "_oob"
        if (variable == "optimal_cutpoint") {
            suffix <- ""
            in_bag = TRUE
        }
        variable <- paste0(variable, suffix)
        variable <- x %>%
            dplyr::select(.data$boot) %>%
            tidyr::unnest(cols = .data$boot) %>%
            dplyr::pull(variable) %>%
            unlist()
        values <- stats::quantile(variable, probs = c(alpha / 2, 1 - alpha / 2),
                           na.rm = TRUE)
        res <- tibble::tibble(
            quantile = c(alpha / 2, 1 - alpha / 2),
            values = unname(values)
        )
        return(res)
    } else {
        res <- purrr::map2_dfr(.x = x$subgroup, .y = x$boot,
                               .f = function(s, b) {
            variable <- rlang::enquo(variable)
            variable <- rlang::as_name(variable)
            if (in_bag) suffix <- "_b" else suffix <- "_oob"
            if (variable == "optimal_cutpoint") {
                suffix <- ""
                in_bag = TRUE
            }
            variable <- paste0(variable, suffix)
            variable <- b %>%
                dplyr::pull(variable) %>%
                unlist()
            values <- stats::quantile(variable, probs = c(alpha / 2, 1 - alpha / 2),
                               na.rm = TRUE)
            tibble::tibble(
                subgroup = s,
                quantile = c(alpha / 2, 1 - alpha / 2),
                values = unname(values)
            )
        })
        return(res)
    }
}