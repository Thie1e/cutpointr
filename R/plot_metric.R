#' @export
plot_metric <- function(x) {
    stopifnot("cutpointr" %in% class(x))
    if (suppressWarnings(is.null(x$roc_curve[[1]]$m))) {
        stop(paste("The cutpointr object does not include a metric column in",
                   "roc_curve - maybe because a method other than",
                   "maximize_metric or minimize_metric was used"))
    }
    res_unnested <- x %>%
        dplyr::select_(.data = ., .dots = c("roc_curve", "subgroup")) %>%
        tidyr::unnest()
    p <- ggplot2::ggplot(res_unnested, ggplot2::aes_(x = ~ x.sorted,
                                                     y = ~ m,
                                                     color = ~ subgroup)) +
        ggplot2::geom_line() + ggplot2::geom_point() +
        ggplot2::ggtitle("Metric values by cutpoint value") +
        ggplot2::ylab("Metric") + ggplot2::xlab("Cutpoint")
    return(p)
}