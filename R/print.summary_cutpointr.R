#' @export
#' @importFrom stats sd
print.summary_cutpointr <- function(x, ...) {
    if (suppressWarnings(is.null(x$subgroup))) {
        cat(paste("Optimal cutpoints using method:", unique(x$method), "\n"))
        cat(paste0(rep("-", getOption("width")), collapse = ""), "\n")
        cps <- round(x$optimal_cutpoint, 5)
        for (i in 1:length(cps)) {
            cat(paste0("Variable ", x$predictor[i], ": ",
                       cps[i],
                       " with ", colnames(x)[3], " = ", round(x[i, 3], 4),
                       "\n"))
        }

        cat("\n")

        desc <- purrr::map(x$data, function(d) {
            c(summary(d$x), SD = round(sd(d$x), 2))
        })
        cat("Descriptive statistics \n")
        cat(paste0(rep("-", getOption("width")), collapse = ""), "\n")
        n_obs <- purrr::map_int(x$data, nrow)
        for (i in 1:length(n_obs)) {
            cat(paste0(n_obs[i], " observations, prevalence ",
                       round(x$prevalence[i], 4) * 100,
                       "%", " \n"))
        }
        cat("\n")
        desc <- do.call(rbind, desc)
        rownames(desc) <- ""
        print(desc)

        cat("\n")

        if (!is.null(suppressWarnings(x$boot))) {
            cat("Bootstrap estimates \n")
            cat(paste0(rep("-", getOption("width")), collapse = ""), "\n")
            cat("...of optimal cutpoint \n")
            cut_b <- purrr::map(x$boot, function(b) {
                c(summary(b$optimal_cutpoint),
                  SD = round(sd(b$optimal_cutpoint), 2))
            })
            cut_b <- do.call(rbind, cut_b)
            rownames(cut_b) <- ""
            print(cut_b)

            cat("\n")
            cat(paste("...of", find_metric_name(colnames(x)), "\n"))
            metric_b <- purrr::map(x$boot, function(b) {
                metric_name <- find_metric_name(colnames(b))
                summary(unlist(b[, metric_name]))
            })
            metric_b <- do.call(rbind, metric_b)
            rownames(metric_b) <- ""
            print(metric_b)
        }
    } else {
        # Grouped data:
        cat(paste("Optimal cutpoints using method:", unique(x$method), "\n"))
        cat(paste0(rep("-", getOption("width")), collapse = ""), "\n")
        cps <- round(x$optimal_cutpoint, 5)
        for (i in 1:length(cps)) {
            cat(paste0("Subgroup ", x$subgroup[i],
                       ", variable ", x$predictor[i], ": ",
                       cps[i],
                       " with ", colnames(x)[4], " = ", round(x[i, 4], 4),
                       "\n"))
        }

        cat("\n")

        desc <- purrr::map(x$data, function(d) {
            c(summary(d$x), SD = round(sd(d$x), 2))
        })
        cat("Descriptive statistics \n")
        cat(paste0(rep("-", getOption("width")), collapse = ""), "\n")
        n_obs <- purrr::map_int(x$data, nrow)
        for (i in 1:length(n_obs)) {
            cat(paste0("Subgroup ", x$subgroup[i], ": ", n_obs[i],
                       " observations, prevalence ",
                       round(x$prevalence[i], 4) * 100,
                       "%", " \n"))
        }
        cat("\n")
        desc <- do.call(rbind, desc)
        rownames(desc) <- x$subgroup
        print(desc)

        cat("\n")

        if (!is.null(suppressWarnings(x$boot))) {
            cat("Bootstrap estimates \n")
            cat(paste0(rep("-", getOption("width")), collapse = ""), "\n")
            cat("...of optimal cutpoint \n")
            cut_b <- purrr::map(x$boot, function(b) {
                c(summary(b$optimal_cutpoint),
                  SD = round(sd(b$optimal_cutpoint), 2))
            })
            cut_b <- do.call(rbind, cut_b)
            rownames(cut_b) <- x$subgroup
            print(cut_b)

            cat("\n")
            cat(paste("...of", find_metric_name(colnames(x)), "\n"))
            metric_b <- purrr::map(x$boot, function(b) {
                metric_name <- find_metric_name(colnames(b))
                summary(unlist(b[, metric_name]))
            })
            metric_b <- do.call(rbind, metric_b)
            rownames(metric_b) <- x$subgroup
            print(metric_b)
        }
    }
}




