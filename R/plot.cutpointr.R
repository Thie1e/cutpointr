#' @export
plot.cutpointr <- function(cutpointr, ...) {

    args <- list(...)

    if (is.null(suppressWarnings(cutpointr$subgroup))) {
        dts_boot <- "boot"
        dts <- "data"
        fll <- NULL
        clr <- NULL
        transparency <- 1
    } else {
        dts_boot <- c("boot", "subgroup")
        dts <- c("data", "subgroup")
        fll <- ~ subgroup
        clr <- ~ subgroup
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
                            ggplot2::aes_(x = ~ optimal_cutpoint, fill = fll, color = clr)) +
                ggplot2::geom_density(alpha = transparency) +
                ggplot2::geom_rug(alpha = 0.5) +
                ggplot2::ggtitle("Bootstrap", "distribution of optimal cutpoints") +
                ggplot2::xlab("optimal cutpoint") +
                ggplot2::theme(legend.position = "none")
        )
        metric_name <- find_metric_name(colnames(res_boot_unnested))
        boot_metric <- suppressMessages(
            ggplot2::ggplot(res_boot_unnested,
                            ggplot2::aes_(x = ~ get(metric_name), fill = fll, color = clr)) +
                ggplot2::geom_density(alpha = transparency) +
                ggplot2::geom_rug(alpha = 0.5) +
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
    if (is.null(suppressWarnings(cutpointr$subgroup))) {
        res_unnested$optimal_cutpoint <- cutpointr$optimal_cutpoint
        col <- NULL
    } else {
        res_unnested <- dplyr::full_join(res_unnested,
                                  cutpointr[, c("optimal_cutpoint", "subgroup")],
                                  by = "subgroup")
        col <- ~ subgroup
    }
    dist <- ggplot2::ggplot(res_unnested, ggplot2::aes_(x = ~ x, fill = fll, color = clr)) +
        ggplot2::geom_density(alpha = transparency) +
        ggplot2::geom_rug(alpha = 0.5) +
        ggplot2::geom_vline(ggplot2::aes_(xintercept = ~ optimal_cutpoint,
                                          color = col),
                            show.legend = FALSE) +
        ggplot2::facet_grid(~ class) + # facet by class because always 2
        ggplot2::ggtitle("Independent variable",
                         "distribution by class and optimal cutpoint") +
        ggplot2::xlab("value") +
        ggplot2::theme(legend.position = "none")

    if (cutpointr$direction[1] == "<") res_unnested$x <- -res_unnested$x
    res_unnested <- res_unnested %>%
        ### pos_class should all be the same, maybe map over rows would be cleaner
        dplyr::mutate_(class = ~ ifelse(class == cutpointr$pos_class[1], 1, 0))
    roc <- ggplot2::ggplot(res_unnested,
                           ggplot2::aes_(m = ~ x, d = ~ class,
                                         color = clr)) +
        suppressWarnings(plotROC::geom_roc(n.cuts = 0,# lineend = "round",
                                           linealpha = transparency)) +
        ggplot2::ggtitle("ROC curve", "by class") +
        ggplot2::xlab("1 - Specificity") +
        ggplot2::ylab("Sensitivity") +
        ggplot2::theme(legend.position = "none") +
        ggplot2::coord_equal()

    #
    # Compose plots
    #
    plots <- list(dist, roc, boot_cut, boot_metric)
    keep <- !(purrr::map_lgl(plots, is.null))
    plots <- plots[keep]
    plots <- lapply(plots, function(p) p + args)
    rows <- round(sum(keep) / 2)
    pos <- ifelse(rows > 1, "right", "bottom")
    suppressMessages(grid_arrange_shared_legend(plots,
                                                nrow = rows, ncol = 2,
                                                position = pos))
}
