plot.cutpointr <- function(cutpointr) {

    if (is.null(suppressWarnings(cutpointr$group))) {
        dts_boot <- "boot"
        dts <- "data"
        fll <- NULL
        clr <- NULL
        transparency <- 1
    } else {
        dts_boot <- c("boot", "group")
        dts <- c("data", "group")
        fll <- ~ group
        clr <- ~ group
        transparency <- 0.6
    }

    #
    # Bootstrap results
    #
    boot_flag <- !is.null(suppressWarnings(cutpointr$boot))
    if (boot_flag) {
        res_boot_unnested <- cutpointr %>%
            dplyr::select_(.data = ., .dots = dts_boot) %>%
            tidyr::unnest()
        boot_cut <- suppressMessages(
            ggplot2::ggplot(res_boot_unnested,
                            ggplot2::aes_(x = ~ optimal_cutpoint, fill = fll)) +
                ggplot2::geom_histogram(alpha = transparency) +
                ggplot2::ggtitle("Bootstrap", "distribution of optimal cutpoints") +
                ggplot2::xlab("optimal cutpoint") +
                ggplot2::theme(legend.position = "none")
        )
        # Look for metric column
        metric_name <- colnames(res_boot_unnested)
        other_cols <- "group|optimal_cutpoint|Sens|Spec"
        metric_name <- metric_name[!grepl(pattern = other_cols, x = metric_name)]
        metric_name <- metric_name[1] # If multiple metrics / other cols
        boot_metric <- suppressMessages(
            ggplot2::ggplot(res_boot_unnested,
                            ggplot2::aes_(x = ~ get(metric_name), fill = fll)) +
                ggplot2::geom_histogram(alpha = transparency) +
                ggplot2::ggtitle("Bootstrap",
                                 paste("out-of-bag estimates of", metric_name)) +
                ggplot2::xlab(metric_name) +
                ggplot2::theme(legend.position = "none")
        )
    } else {
        boot_cut    <- NULL
        boot_metric <- NULL
    }

    #
    # In-sample results
    #
    res_unnested <- cutpointr %>%
        dplyr::select_(.data = ., .dots = dts) %>%
        tidyr::unnest()
    dist <- ggplot2::ggplot(res_unnested, ggplot2::aes_(x = ~ x, fill = fll)) +
        ggplot2::geom_density(alpha = transparency) +
        ggplot2::facet_grid(~ class) + # facet by class because always 2
        ggplot2::ggtitle("Distribution of independent variable", "by class") +
        ggplot2::xlab("value") +
        ggplot2::theme(legend.position = "none")

    res_unnested <- res_unnested %>%
        ### pos_class should all be the same
        ### maybe map over rows would be cleaner
        mutate_(class = ~ ifelse(class == cutpointr$pos_class[1], 1, 0))
    if (suppressWarnings(!is.null(cutpointr$group))) {
        roc <- ggplot2::ggplot(res_unnested,
                               ggplot2::aes_(m = ~ x, d = ~ class,
                                             color = clr)) +
            plotROC::geom_roc() +
            ggplot2::ggtitle("ROC curve", "by class") +
            ggplot2::xlab("1 - Specificity") +
            ggplot2::ylab("Sensitivity") +
            ggplot2::theme(legend.position = "none")
    } else {
        ##### delete this? ------------------------
        roc <- ggplot2::ggplot(res_unnested,
                               ggplot2::aes_(m = ~ x, d = ~ class)) +
            plotROC::geom_roc() +
            ggplot2::ggtitle("ROC curve", "by class") +
            ggplot2::xlab("1 - Specificity") +
            ggplot2::ylab("Sensitivity") +
            ggplot2::theme(legend.position = "none")
        ##### ----------------------------
    }

    #
    # Compose plots
    #
    plots <- list(dist, roc, boot_cut, boot_metric)
    keep <- !(purrr::map_lgl(plots, is.null))
    plots <- plots[keep]
    # suppressMessages(multiggplot(plots)) ### Add plot.layout depending on number of included plots
    rows <- round(sum(keep) / 2)
    pos <- ifelse(rows > 1, "right", "bottom")
    suppressMessages(grid_arrange_shared_legend(plots,
                                                nrow = rows, ncol = 2,
                                                position = pos))
}
