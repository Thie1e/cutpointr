plot.cutpointr <- function(cutpointr) {

    boot_flag <- !is.null(suppressWarnings(cutpointr$boot))
    if (boot_flag) {
        res_boot_unnested <- cutpointr %>%
            dplyr::select_(.data = ., .dots = c("boot", "group")) %>%
            tidyr::unnest()
        boot_cut <- suppressMessages(
            ggplot2::ggplot(res_boot_unnested, ggplot2::aes_(x = ~ optcut_b, fill = ~ group)) +
                ggplot2::geom_histogram(alpha = 0.66) +
                ggplot2::ggtitle("Bootstrap", "distribution of optimal cutpoints") +
                ggplot2::xlab("optimal cutpoint")
        )
    } else {
        boot_cut <- NULL
    }

    res_unnested <- cutpointr %>%
        dplyr::select_(.data = ., .dots = c("group", "data")) %>%
        tidyr::unnest()
    dist <- ggplot2::ggplot(res_unnested, ggplot2::aes_(x = ~ x, fill = ~ group)) +
        # ggplot2::geom_histogram(alpha = 0.5) +
        ggplot2::geom_density(alpha = 0.66) +
        ggplot2::ggtitle("Distribution of independent variable") +
        ggplot2::xlab("value")

    res_unnested <- res_unnested %>%
        mutate_(class = ~ ifelse(class == cutpointr$pos_class[1], 1, 0)) ### pos_class all be the same
        ### maybe map over rows would be cleaner
    if (suppressWarnings(!is.null(cutpointr$group))) {
        roc <- ggplot2::ggplot(res_unnested,
                               ggplot2::aes_(m = ~ x, d = ~ class,
                                             color = ~ group)) +
            plotROC::geom_roc()
    } else {
        roc <- ggplot2::ggplot(res_unnested,
                               ggplot2::aes_(m = ~ x, d = ~ class)) +
            plotROC::geom_roc()
    }

    ### Add plot of distribution of LOO-Boot estimate of metric that the
    ### optcut func optimizes

    plots <- list(dist, boot_cut, roc)
    keep <- !(purrr::map_lgl(plots, is.null))
    plots <- plots[keep]
    multiggplot(plots) ### Add plot.layout depending on number of included plots
}
