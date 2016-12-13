#' @export
print.summary_cutpointr <- function(cutpointr) {
    cat(paste("Optimal cutpoints using method:", unique(cutpointr$method), "\n"))
    cat(c(stringi::stri_pad_right("-", pad = "-"), "\n"))

    n_obs <- purrr::map_int(cutpointr$data, nrow)
    for (i in 1:length(n_obs)) {
        cat(paste0("Group ", cutpointr$group[i], ": ", n_obs[i],
                   " observations, prevalence ",
                   round(cutpointr$prevalence[i], 4) * 100,
                   "%", " \n"))
    }

    cat("\n")

    desc <- purrr::map(cutpointr$data, function(d) {
        c(summary(d$x), SD = round(sd(d$x), 2))
    })
    cat("Descriptive statistics \n")
    cat(c(stringi::stri_pad_right("-", pad = "-"), "\n"))
    for (i in 1:length(desc)) {
        cat(paste0("Group ", cutpointr$group[i], ": ",
                   "Min. ", desc[[i]]["Min."], ";",
                   " 1st Qu. ", desc[[i]]["1st Qu."], ";",
                   " Mean ", desc[[i]]["Mean"], ";",
                   " 3rd Qu. ", desc[[i]]["3rd Qu."], ";",
                   " Max ", desc[[i]]["Max."], ";",
                   " SD ", desc[[i]]["Max."],
                   " \n"))
    }

    cat("\n")

    cat("Optimal cutpoint \n")
    cat(c(stringi::stri_pad_right("-", pad = "-"), "\n"))
    cps <- cutpointr$optimal_cutpoint
    for (i in 1:length(cps)) {
        cat(paste0("Group ", cutpointr$group[i], ": ",
                   cps[i], " based on method ",
                   cutpointr$method[i]), "\n")
    }

    cat("\n")

    if (!is.null(suppressWarnings(cutpointr$boot))) {
        cat("Bootstrap estimates \n")
        cat(c(stringi::stri_pad_right("-", pad = "-"), "\n"))
        cat("...of optimal cutpoint \n")
        cut_b <- purrr::map(cutpointr$boot, function(b) {
            c(summary(b$optimal_cutpoint),
              SD = round(sd(b$optimal_cutpoint), 2))
        })
        for (i in 1:length(cut_b)) {
            cat(paste0("Group ", cutpointr$group[i], ": ",
                       "Min. ", cut_b[[i]]["Min."], ";",
                       " 1st Qu. ", cut_b[[i]]["1st Qu."], ";",
                       " Mean ", cut_b[[i]]["Mean"], ";",
                       " 3rd Qu. ", cut_b[[i]]["3rd Qu."], ";",
                       " Max ", cut_b[[i]]["Max."], ";",
                       " Max ", cut_b[[i]]["Max."], ";",
                       " SD ", cut_b[[i]]["SD"],
                       " \n"))
        }
        cat("\n")
        cat(paste("...of", find_metric_name(colnames(cutpointr)), "\n"))
        metric_b <- purrr::map(cutpointr$boot, function(b) {
            metric_name <- find_metric_name(colnames(b))
            summary(unlist(b[, metric_name]))
        })
        for (i in 1:length(metric_b)) {
            cat(paste0("Group ", cutpointr$group[i], ": ",
                       "Min. ", metric_b[[i]]["Min."], ";",
                       " 1st Qu. ", metric_b[[i]]["1st Qu."], ";",
                       " Mean ", metric_b[[i]]["Mean"], ";",
                       " 3rd Qu. ", metric_b[[i]]["3rd Qu."], ";",
                       " Max ", metric_b[[i]]["Max."], ";",
                       " Max ", metric_b[[i]]["Max."],
                       " \n"))
        }
    }
}




