#' @export
plot.cutpointr <- function(x, ...) {

    args <- list(...)

    if (is.null(suppressWarnings(x$subgroup))) {
        dts_boot <- "boot"
        dts_roc <- "roc_curve"
        dts <- "data"
        fll <- NULL
        clr <- NULL
        transparency <- 1
    } else {
        dts_boot <- c("boot", "subgroup")
        dts_roc <- c("roc_curve", "subgroup")
        dts <- c("data", "subgroup")
        fll <- ~ subgroup
        clr <- ~ subgroup
        transparency <- 0.6
    }

    #
    # Bootstrap results
    #
    boot_flag <- !is.null(suppressWarnings(x$boot))
    if (boot_flag) {
        res_boot_unnested <- x %>%
            dplyr::select_(.data = ., .dots = dts_boot) %>%
            tidyr::unnest()
        if (all(na.omit(res_boot_unnested$optimal_cutpoint %% 1 == 0))) {
            dist_plot <- ggplot2::geom_histogram(alpha = transparency, position = "identity")
        } else {
            dist_plot <- ggplot2::geom_density(alpha = transparency)
        }
        boot_cut <- suppressMessages(
            ggplot2::ggplot(res_boot_unnested,
                            ggplot2::aes_(x = ~ optimal_cutpoint, fill = fll, color = clr)) +
                dist_plot +
                ggplot2::geom_rug(alpha = 0.5) +
                ggplot2::ggtitle("Bootstrap", "distribution of optimal cutpoints") +
                ggplot2::xlab("optimal cutpoint") +
                ggplot2::theme(legend.position = "none")
        )
        met_ind <- which(colnames(res_boot_unnested) == "optimal_cutpoint") + 1
        metric_name <- colnames(res_boot_unnested)[met_ind]
        if (all(na.omit(get(metric_name, res_boot_unnested) %% 1 == 0))) {
            dist_plot <- ggplot2::geom_histogram(alpha = transparency, position = "identity")
        } else {
            dist_plot <- ggplot2::geom_density(alpha = transparency)
        }
        boot_metric <- suppressMessages(
            ggplot2::ggplot(res_boot_unnested,
                            ggplot2::aes_(x = ~ get(metric_name), fill = fll, color = clr)) +
                dist_plot +
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
    res_unnested <- x %>%
        dplyr::select_(.data = ., .dots = dts) %>%
        tidyr::unnest()
    if (is.null(suppressWarnings(x$subgroup))) {
        res_unnested$optimal_cutpoint <- x$optimal_cutpoint
        col <- NULL
    } else {
        res_unnested <- dplyr::full_join(res_unnested,
                                  x[, c("optimal_cutpoint", "subgroup")],
                                  by = "subgroup")
        col <- ~ subgroup
    }
    if (all(na.omit(res_unnested$x %% 1 == 0))) {
        dist_plot <- ggplot2::geom_histogram(alpha = transparency, position = "identity")
    } else {
        dist_plot <- ggplot2::geom_density(alpha = transparency)
    }
    dist <- ggplot2::ggplot(res_unnested,
                            ggplot2::aes_(x = ~ x, fill = fll, color = clr)) +
        dist_plot +
        ggplot2::geom_rug(alpha = 0.5) +
        ggplot2::geom_vline(ggplot2::aes_(xintercept = ~ optimal_cutpoint,
                                          color = col),
                            show.legend = FALSE) +
        # facet by class because always 2
        ggplot2::facet_wrap(~ class, scales = "free_y") +
        ggplot2::ggtitle("Independent variable",
                         "distribution by class and optimal cutpoint") +
        ggplot2::xlab("value") +
        ggplot2::theme(legend.position = "none")

    # if (x$direction[1] == "<=" | x$direction[1] == "<") res_unnested$x <- -res_unnested$x
    if (suppressWarnings(is.null(x$subgroup))) {
        roc_title <- ggplot2::ggtitle("ROC curve")
    } else {
        roc_title <- ggplot2::ggtitle("ROC curve", "by class")
    }
    if (any(!is.finite(res_unnested$x))) warning("Infinite values excluded from ROC curve (?)")
    if (x$direction[1] == ">=") {
        optcut_coords <- apply(x, 1, function(r) {
            opt_ind <- which(r$roc_curve$x.sorted <= r$optimal_cutpoint)[1]
            data.frame(tpr = r$roc_curve$tpr[opt_ind], tnr = r$roc_curve$tnr[opt_ind])
        })
    } else if (x$direction[1] == "<=") {
        optcut_coords <- apply(x, 1, function(r) {
            opt_ind <- which(r$roc_curve$x.sorted >= r$optimal_cutpoint)[1]
            data.frame(tpr = r$roc_curve$tpr[opt_ind], tnr = r$roc_curve$tnr[opt_ind])
        })
    }
    optcut_coords <- do.call(rbind, optcut_coords)
    res_unnested <- x %>%
        dplyr::select_(.data = ., .dots = dts_roc) %>%
        tidyr::unnest()
    roc <- ggplot2::ggplot(res_unnested,
                           ggplot2::aes_(x = ~ 1 - tnr, y = ~ tpr, color = clr)) +
        ggplot2::geom_line() +
        ggplot2::geom_point(data = optcut_coords, color = "black") +
        roc_title +
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
