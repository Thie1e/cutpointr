#' @export
print.summary_cutpointr <- function(cutpointr) {
    if (suppressWarnings(is.null(cutpointr$group))) {
        stop("Not yet implemented for ungrouped cutpointr object")
    }

    cat(paste("Optimal cutpoints using method:", unique(cutpointr$method), "\n"))
    cat(paste0(rep("-", getOption("width")), collapse = ""), "\n")
    cps <- round(cutpointr$optimal_cutpoint, 5)
    for (i in 1:length(cps)) {
        cat(paste0("Group ", cutpointr$group[i], ": ",
                   cps[i], "\n"))
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
    # for (i in 1:length(desc)) {
    #     cat(paste0("Group ", cutpointr$group[i], ": ",
    #                "Min. ", desc[[i]]["Min."], ";",
    #                " 1st Qu. ", desc[[i]]["1st Qu."], ";",
    #                " Mean ", desc[[i]]["Mean"], ";",
    #                " 3rd Qu. ", desc[[i]]["3rd Qu."], ";",
    #                " Max ", desc[[i]]["Max."], ";",
    #                " SD ", desc[[i]]["Max."],
    #                " \n"))
    # }

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
        # for (i in 1:length(cut_b)) {
        #     cat(paste0("Group ", cutpointr$group[i], ": ",
        #                "Min. ", cut_b[[i]]["Min."], ";",
        #                " 1st Qu. ", cut_b[[i]]["1st Qu."], ";",
        #                " Mean ", cut_b[[i]]["Mean"], ";",
        #                " 3rd Qu. ", cut_b[[i]]["3rd Qu."], ";",
        #                " Max ", cut_b[[i]]["Max."], ";",
        #                " Max ", cut_b[[i]]["Max."], ";",
        #                " SD ", cut_b[[i]]["SD"],
        #                " \n"))
        # }
        cat("\n")
        cat(paste("...of", find_metric_name(colnames(cutpointr)), "\n"))
        metric_b <- purrr::map(cutpointr$boot, function(b) {
            metric_name <- find_metric_name(colnames(b))
            summary(unlist(b[, metric_name]))
        })
        metric_b <- do.call(rbind, metric_b)
        rownames(metric_b) <- cutpointr$group
        print(metric_b)
        # for (i in 1:length(metric_b)) {
        #     cat(paste0("Group ", cutpointr$group[i], ": ",
        #                "Min. ", metric_b[[i]]["Min."], ";",
        #                " 1st Qu. ", metric_b[[i]]["1st Qu."], ";",
        #                " Mean ", metric_b[[i]]["Mean"], ";",
        #                " 3rd Qu. ", metric_b[[i]]["3rd Qu."], ";",
        #                " Max ", metric_b[[i]]["Max."], ";",
        #                " Max ", metric_b[[i]]["Max."],
        #                " \n"))
        # }
    }
}




