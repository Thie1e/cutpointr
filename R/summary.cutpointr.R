#' @export
# summary.cutpointr <- function(object, ...) {
#     class(object) <- c("summary_cutpointr", class(object))
#     return(object)
# }

#' @export
#' @importFrom stats sd
summary.cutpointr <- function(x, ...) {
    x_summary <- list()
    if (suppressWarnings(is.null(x$subgroup))) {
        cps <- round(x$optimal_cutpoint, 5)
        for (i in 1:length(cps)) {
            cat(paste0("Variable ", x$predictor[i], ": ",
                       cps[i],
                       " with ", colnames(x)[3], " = ", round(x[i, 3], 4),
                       "\n"))
        }
        x_summary$cps <- cps

        desc <- purrr::map(x$data, function(d) {
            c(summary(d$x), SD = round(sd(d$x), 2))
        })
        n_obs <- purrr::map_int(x$data, nrow)
        x_summary$n_obs <- n_obs
        for (i in 1:length(n_obs)) {
            cat(paste0(n_obs[i], " observations, prevalence ",
                       round(x$prevalence[i], 4) * 100,
                       "%", " \n"))
        }
        desc <- do.call(rbind, desc)
        rownames(desc) <- ""
        x_summary$desc <- desc

        if (!is.null(suppressWarnings(x$boot))) {
            cut_b <- purrr::map(x$boot, function(b) {
                c(summary(b$optimal_cutpoint),
                  SD = round(sd(b$optimal_cutpoint), 2))
            })
            cut_b <- do.call(rbind, cut_b)
            rownames(cut_b) <- ""
            x_summary$cut_b <- cut_b

            metric_b <- purrr::map(x$boot, function(b) {
                metric_name <- find_metric_name(colnames(b))
                summary(unlist(b[, metric_name]))
            })
            metric_b <- do.call(rbind, metric_b)
            rownames(metric_b) <- ""
            x_summary$metric_b <- metric_b
        }
    } else {
        # Grouped data:
        cps <- round(x$optimal_cutpoint, 5)
        x_summary$cps <- cps

        desc <- purrr::map(x$data, function(d) {
            c(summary(d$x), SD = round(sd(d$x), 2))
        })
        n_obs <- purrr::map_int(x$data, nrow)
        x_summary$n_obs <- n_obs
        desc <- do.call(rbind, desc)
        rownames(desc) <- x$subgroup
        x_summary$desc <- desc

        if (!is.null(suppressWarnings(x$boot))) {
            cut_b <- purrr::map(x$boot, function(b) {
                c(summary(b$optimal_cutpoint),
                  SD = round(sd(b$optimal_cutpoint), 2))
            })
            cut_b <- do.call(rbind, cut_b)
            rownames(cut_b) <- x$subgroup
            x_summary$cut_b <- cut_b

            metric_b <- purrr::map(x$boot, function(b) {
                metric_name <- find_metric_name(colnames(b))
                summary(unlist(b[, metric_name]))
            })
            metric_b <- do.call(rbind, metric_b)
            rownames(metric_b) <- x$subgroup
            x_summary$metric_b <- metric_b
        }
    }
    class(x_summary) <- c("summary_cutpointr")
    return(x_summary)
}




