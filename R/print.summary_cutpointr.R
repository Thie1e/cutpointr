#' @export
print.summary_cutpointr <- function(cutpointr) {
    if (suppressWarnings(is.null(cutpointr$group))) {
        cat(paste("Optimal cutpoints using method:", unique(cutpointr$method), "\n"))
        cat(paste0(rep("-", getOption("width")), collapse = ""), "\n")
        cps <- round(cutpointr$optimal_cutpoint, 5)
        for (i in 1:length(cps)) {
            cat(paste0("Variable ", cutpointr$predictor[i], ": ",
                       cps[i],
                       " with ", colnames(cutpointr)[3], " = ", round(cutpointr[i, 3], 4),
                       "\n"))
        }

        cat("\n")

        desc <- purrr::map(cutpointr$data, function(d) {
            c(summary(d$x), SD = round(sd(d$x), 2))
        })
        cat("Descriptive statistics \n")
        cat(paste0(rep("-", getOption("width")), collapse = ""), "\n")
        n_obs <- purrr::map_int(cutpointr$data, nrow)
        for (i in 1:length(n_obs)) {
            cat(paste0(n_obs[i], " observations, prevalence ",
                       round(cutpointr$prevalence[i], 4) * 100,
                       "%", " \n"))
        }
        cat("\n")
        desc <- do.call(rbind, desc)
        rownames(desc) <- ""
        print(desc)

        cat("\n")

        if (!is.null(suppressWarnings(cutpointr$boot))) {
            cat("Bootstrap estimates \n")
            cat(paste0(rep("-", getOption("width")), collapse = ""), "\n")
            cat("...of optimal cutpoint \n")
            cut_b <- purrr::map(cutpointr$boot, function(b) {
                c(summary(b$optimal_cutpoint),
                  SD = round(sd(b$optimal_cutpoint), 2))
            })
            cut_b <- do.call(rbind, cut_b)
            rownames(cut_b) <- ""
            print(cut_b)

            cat("\n")
            cat(paste("...of", find_metric_name(colnames(cutpointr)), "\n"))
            metric_b <- purrr::map(cutpointr$boot, function(b) {
                metric_name <- find_metric_name(colnames(b))
                summary(unlist(b[, metric_name]))
            })
            metric_b <- do.call(rbind, metric_b)
            rownames(metric_b) <- ""
            print(metric_b)
        }
    } else {
        # Grouped data:
        cat(paste("Optimal cutpoints using method:", unique(cutpointr$method), "\n"))
        cat(paste0(rep("-", getOption("width")), collapse = ""), "\n")
        cps <- round(cutpointr$optimal_cutpoint, 5)
        for (i in 1:length(cps)) {
            cat(paste0("Group ", cutpointr$group[i],
                       ", variable ", cutpointr$predictor[i], ": ",
                       cps[i],
                       " with ", colnames(cutpointr)[4], " = ", round(cutpointr[i, 4], 4),
                       "\n"))
        }

        cat("\n")

        desc <- purrr::map(cutpointr$data, function(d) {
            c(summary(d$x), SD = round(sd(d$x), 2))
        })
        cat("Descriptive statistics \n")
        cat(paste0(rep("-", getOption("width")), collapse = ""), "\n")
        n_obs <- purrr::map_int(cutpointr$data, nrow)
        for (i in 1:length(n_obs)) {
            cat(paste0("Group ", cutpointr$group[i], ": ", n_obs[i],
                       " observations, prevalence ",
                       round(cutpointr$prevalence[i], 4) * 100,
                       "%", " \n"))
        }
        cat("\n")
        desc <- do.call(rbind, desc)
        rownames(desc) <- cutpointr$group
        print(desc)

        cat("\n")

        if (!is.null(suppressWarnings(cutpointr$boot))) {
            cat("Bootstrap estimates \n")
            cat(paste0(rep("-", getOption("width")), collapse = ""), "\n")
            cat("...of optimal cutpoint \n")
            cut_b <- purrr::map(cutpointr$boot, function(b) {
                c(summary(b$optimal_cutpoint),
                  SD = round(sd(b$optimal_cutpoint), 2))
            })
            cut_b <- do.call(rbind, cut_b)
            rownames(cut_b) <- cutpointr$group
            print(cut_b)

            cat("\n")
            cat(paste("...of", find_metric_name(colnames(cutpointr)), "\n"))
            metric_b <- purrr::map(cutpointr$boot, function(b) {
                metric_name <- find_metric_name(colnames(b))
                summary(unlist(b[, metric_name]))
            })
            metric_b <- do.call(rbind, metric_b)
            rownames(metric_b) <- cutpointr$group
            print(metric_b)
        }
    }
}




