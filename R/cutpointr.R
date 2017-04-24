#' Determine and evaluate optimal cutpoints
#'
#' Using predictions (or e.g. biological marker values) and binary class labels, this function
#' will determine "optimal" cutpoints using various selectable methods. The
#' methods for cutpoint determination can be evaluated using bootstrapping. An
#' estimate of the cutpoint variability and the out-of-sample performance will
#' be returned.
#'
#' If direction and/or pos_class and neg_class are not given, the function will
#' assume that higher values indicate the positive class and assign the class
#' with a higher mean as the positive class.
#'
#' Different methods can be used for determining the "optimal" cutpoint via
#' the method argument. The package includes the following cutpoint functions:
#' \itemize{
#'  \item maximize_metric: Maximize the metric function
#'  \item minimize_metric: Minimize the metric function
#'  \item oc_manual: Specify the cutoff value manually
#'  \item oc_youden_kernel: Maximize the Youden-Index after kernel smoothing
#'  the distributions of the two classes
#'  \item oc_youden_normal: Maximize the Youden-Index parametrically
#'  assuming normally distributed data in both classes
#'  \item oc_OptimalCutpoints: A wrapper for optimal.cutpoints from the OptimalCutpoints package.
#'  Supply an additional "oc_metric" argument with the method choice corresponding
#'  to a method from the OptimalCutpoints package
#' }
#'
#' User defined functions can be supplied to method, too. As a reference,
#' the code of all included method functions can be accessed by simply typing
#' their name. To define a new method function, create a function that may take
#' as input(s):
#' \itemize{
#'  \item data: A data frame or tbl_df
#'  \item x: (character) The name of the predictor or independent variable
#'  \item class: (character) The name of the class or dependent variable
#'  \item metric_func: A function for calculating a metric, e.g. accuracy. Note
#'  that the method function does not necessarily have to accept this argument
#'  \item pos_class: The positive class
#'  \item neg_class: The negative class
#'  \item direction: ">=" if the positive class has higher x values, "<=" otherwise
#' }
#'
#' The ... argument can be used to avoid an error if not all of the above
#' arguments are needed or in oder to pass additional arguments to `method`.
#' The function should return a data frame or tbl_df with
#' one row, the column "optimal_cutpoint", and an optional column with an arbitraty name
#' with the metric value at the optimal cutpoint.
#'
#' Built-in metric functions include:
#' \itemize{
#'  \item accuracy: Fraction correctly classified
#'  \item youden: Youden- or J-Index = sensitivity + specificity - 1
#'  \item sum_sens_spec: sensitivity + specificity
#'  \item cohens_kappa: Cohen's Kappa
#'  \item abs_d_sesp: The absolute difference of sensitivity and specificity
#' }
#'
#' User defined metric functions can be used as well which can accept the following
#' inputs as vectors:
#' \itemize{
#'  \item tp: Vector of true positives
#'  \item fp: Vector of false positives
#'  \item tn: Vector of true negatives
#'  \item fn: Vector of false negatives
#' }
#'
#' The function should return a matrix with one column. If the column is named,
#' the named will be included in the output and plots. Avoid using names that
#' are identical to the column names that are by default returned by cutpointr.
#'
#' If boot_runs is positive, that number of bootstrap samples will be drawn
#' and the optimal cutpoint using method will be determined. Additionally,
#' as a form of cross validation, the function in metric will be used to
#' score the out-of-bag predictions using the cutpoints determined by
#' method. Accuracy,
#' Sensitivity, Specificity, Kappa, true positives/negatives and false
#' positives/negatives are always included in the bootstrap results.
#'
#' If multiple optimal cutpoints are found, the first one is returned and a
#' warning including all optimal cutpoints is issued. The first one refers to
#' the minimum of the optimal cutpoints if direction = ">=" or to the maximum
#' of the optimal cutpoints if direction = "<=".
#'
#' If use_midpoints is set to TRUE and multiple optimal cutpoints are found,
#' the midpoint of the minimum / maximum of the optimal cutpoints
#' and the next highest / lowest observation is returned, as described above. Thus, finding
#' multiple optimal cutpoints has no effect on determining the midpoint.
#'
#' @examples
#' library(cutpointr)
#'
#' ## Optimal cutpoint for dsi
#' data(suicide)
#' opt_cut <- cutpointr(suicide, dsi, suicide)
#' opt_cut
#' summary(opt_cut)
#' plot(opt_cut)
#' predict(opt_cut, newdata = data.frame(dsi = 0:5))
#'
#'## direction, class labels, method and metric can be defined manually
#' opt_cut <- cutpointr(suicide, dsi, suicide, direction = ">=", pos_class = "yes",
#'                      method = maximize_metric, metric = youden)
#' opt_cut
#'
#' ## Optimal cutpoint for dsi, as before, but for the separate subgroups
#' opt_cut <- cutpointr(suicide, dsi, suicide, gender)
#' opt_cut
#' summary(opt_cut)
#' plot(opt_cut)
#'
#' ## Bootstrapping to assess cutpoint variability and out-of-sample performance
#' set.seed(12)
#' opt_cut <- cutpointr(suicide, dsi, suicide, boot_runs = 30)
#' opt_cut
#' summary(opt_cut)
#' plot(opt_cut)
#'
#' ## Bootstrapping also works on individual subgroups
#' set.seed(12)
#' opt_cut <- cutpointr(suicide, dsi, suicide, gender, boot_runs = 30)
#' opt_cut
#' summary(opt_cut)
#' plot(opt_cut)
#'
#' ## Transforming variables (unrealistic, just to show the functionality)
#' set.seed(12)
#' opt_cut <- cutpointr(suicide, log(dsi + 1), suicide == "yes",
#'     subgroup = dsi %% 2 == 0, boot_runs = 30)
#' opt_cut
#' summary(opt_cut)
#' plot(opt_cut)
#' predict(opt_cut, newdata = data.frame(dsi = 1:3))
#'
#' ## Different cutpoint function / metric
#' set.seed(12)
#' opt_cut <- cutpointr(suicide, dsi, suicide, gender, pos_class = "yes",
#'   boot_runs = 30, method = minimize_metric, metric = abs_d_sesp)
#' opt_cut
#' plot(opt_cut)
#'
#' ## Handling of NA values
#' suicide_na <- suicide
#' suicide_na$dsi[10] <- NA
#' suicide_na$suicide[20] <- NA
#' suicide_na$gender[30] <- NA
#' opt_cut_na <- cutpointr(suicide_na, dsi, suicide, gender, na.rm = TRUE)
#' opt_cut_na
#' plot(opt_cut_na)
#'
#' ## Parallelized bootstrapping (warning expected)
#' if (require(doSNOW) & require(doRNG)) {
#'   cl <- makeCluster(2) # 2 cores
#'   registerDoSNOW(cl)
#'   registerDoRNG(12) # Reproducible parallel loops using doRNG
#'   opt_cut <- cutpointr(suicide, dsi, suicide, gender, pos_class = "yes",
#'                  direction = ">=", boot_runs = 100, allowParallel = TRUE)
#'   stopCluster(cl)
#'   opt_cut
#'   plot(opt_cut)
#' }
#'
#'
#' ## Wrapper for optimal.cutpoints
#' if (require(OptimalCutpoints)) {
#'   opt_cut <- cutpointr(suicide, dsi, suicide, gender, boot_runs = 30,
#'                        method = oc_OptimalCutpoints, oc_metric = "Youden")
#'   opt_cut
#'   plot(opt_cut)
#' }
#'
#' ## Cutpoint function assuming normally distributed data
#' opt_cut <- cutpointr(suicide, dsi, suicide, gender, boot_runs = 30,
#'                      method = oc_youden_normal)
#' opt_cut
#' plot(opt_cut)
#'
#'
#'
#' @param data A data frame or tibble in which the columns that are given in x,
#'  class and possibly subgroup can be found.
#' @param x The variable name without quotation marks to be used for
#'  classification, e.g. predictions or test values.
#' @param class The variable name without quotation marks indicating class membership.
#' @param subgroup The variable name without quotation marks
#' of an additional covariate that identifies subgroups. Separate
#' optimal cutpoints will be determined by group. Numeric, character and factor are
#' allowed. Also expressions like z > 10 are possible.
#' @param method (function) A function for determining cutpoints. Can
#' be user supplied or use some of the built in methods. See details.
#' @param metric (function) The function for computing a metric when using
#' maximize_metric or minimize_metric as method and and for the
#' out-of-bag values during bootstrapping. A way of internally validating the performance.
#' User defined functions can be supplied, see details.
#' @param pos_class (optional) The value of class that indicates the positive class.
#' @param neg_class (optional) The value of class that indicates the negative class.
#' @param direction (character, optional) Use ">=" or "<=" to indicate whether x
#' is supposed to be larger or smaller for the positive class.
#' @param boot_runs (numerical) If positive, this number of bootstrap samples
#' will be used to assess the variability and the out-of-sample performance.
#' @param use_midpoints (logical) If TRUE (default FALSE) the returned optimal
#' cutpoint will be the mean of the optimal cutpoint and the next highest
#' observation (for direction = ">") or the next lowest observation
#' (for direction = "<").
#' @param na.rm (logical) Set to TRUE (default FALSE) to keep only complete
#' cases of x, class and subgroup (if specified). Missing values with
#' na.rm = FALSE will raise an error.
#' @param allowParallel (logical) If TRUE, the bootstrapping will be parallelized
#' using foreach. A local cluster, for example, should have been started manually
#' beforehand.
#' @param ... Further optional arguments that will be passed to optcut_func.
#' @importFrom purrr %>%
#' @importFrom foreach %do%
#' @export
cutpointr <- function(data, x, class, subgroup = NULL,
                      method = maximize_metric, metric = sum_sens_spec,
                      pos_class = NULL, neg_class = NULL, direction = NULL,
                      boot_runs = 0, use_midpoints = FALSE, na.rm = FALSE,
                      allowParallel = FALSE, ...) {
    #
    # NSE
    #
    predictor <- deparse(substitute(x))
    outcome   <- deparse(substitute(class))
    x <- eval(substitute(x), data, parent.frame())
    class <- eval(substitute(class), data, parent.frame())
        subgroup_var <- deparse(substitute(subgroup))
        subgroup <- eval(substitute(subgroup), data, parent.frame())

    # Get method function
    if (length(method) > 1 | !(class(method) == "function")) {
        stop("method should be a function")
    } else {
        cl <- match.call()
        mod_name <- cl$method
        # if default was not changed:
        mod_name <- as.character(substitute(method))
    }
    if (is.null(mod_name)) stop("Could not get the method function")

    # Get metric function
    if (length(metric) > 1 | !(class(metric) == "function")) {
        stop("method should be a function")
    }
    cl <- match.call()
    metric_name <- cl$metric
    # if default was not changed:
    metric_name <- as.character(substitute(metric))
    if (is.null(metric_name)) stop("Could not get the metric function")
    res <- list(x, class, subgroup, method, metric, pos_class, neg_class,
                direction, boot_runs, use_midpoints, na.rm, allowParallel,
                predictor, outcome, mod_name, subgroup_var)
    names(res) <- c("x", "class", "subgroup", "method", "metric",
                    "pos_class", "neg_class", "direction", "boot_runs",
                    "use_midpoints", "na.rm", "allowParallel", "predictor",
                    "outcome", "mod_name", "subgroup_var")
    cutpointr_internal(cutpointr_eval = res, ...)
}

#' The standard evaluation version of cutpointr
#'
#' This function is equivalent to cutpointr but takes only quoted arguments
#' for x, class, subgroup, method and metric. This function is suitable for
#' programming. For details on cutpointr see help("cutpointr").
#' @export
#' @inheritParams cutpointr
#' @param x The variable name with quotation marks to be used for
#'  classification, e.g. predictions or test values.
#' @param class The variable name with quotation marks indicating class membership.
#' @param subgroup The variable name with quotation marks
#' of an additional covariate that identifies subgroups. Separate
#' optimal cutpoints will be determined by group. Numeric, character and factor are
#' allowed. Also expressions like z > 10 are possible.
#' @param method (character) A function for determining cutpoints. Can
#' be user supplied or use some of the built in methods. See details.
#' @param metric (character) The function for computing a metric when using
#' maximize_metric or minimize_metric as method and and for the
#' out-of-bag values during bootstrapping. A way of internally validating the performance.
#' User defined functions can be supplied, see details.
cutpointr_ <- function(data, x, class, subgroup = NULL,
                      method = "maximize_metric", metric = "sum_sens_spec",
                      pos_class = NULL, neg_class = NULL, direction = NULL,
                      boot_runs = 0, use_midpoints = FALSE, na.rm = FALSE,
                      allowParallel = FALSE, ...) {
    #
    # SE
    #
    x <- as.name(x)
    class <- as.name(class)
    if (!is.null(subgroup)) subgroup <- as.name(subgroup)
    predictor <- deparse(substitute(x))
    outcome   <- deparse(substitute(class))
    x <- eval(substitute(x), data, parent.frame())
    class <- eval(substitute(class), data, parent.frame())
    subgroup_var <- deparse(substitute(subgroup))
    subgroup <- eval(substitute(subgroup), data, parent.frame())

    # Get method function
    if (length(method) > 1 | !(class(method) == "character")) {
        stop("method should be character string")
    }
    # If a character vec is given the user surely wants to search in the package
    mod_name <- method[1]
    method <- paste0("cutpointr::", method)
    method <- eval(parse(text = method))
    if (is.null(mod_name)) stop("Could not get the method function")

    # Get metric function
    if (length(metric) > 1 | !(class(metric) == "character")) {
        stop("method should be character string")
    }
    # If a character vec is given the user surely wants to search in the package
    metric_name <- metric[1]
    metric <- paste0("cutpointr::", metric)
    metric <- eval(parse(text = metric))
    if (is.null(metric_name)) stop("Could not get the method function")
    res <- list(x, class, subgroup, method, metric, pos_class, neg_class,
                direction, boot_runs, use_midpoints, na.rm, allowParallel,
                predictor, outcome, mod_name, subgroup_var)
    names(res) <- c("x", "class", "subgroup", "method", "metric",
                    "pos_class", "neg_class", "direction", "boot_runs",
                    "use_midpoints", "na.rm", "allowParallel", "predictor",
                    "outcome", "mod_names", "subgroup_var")
    cutpointr_internal(cutpointr_eval = res, ...)
}


cutpointr_internal <- function(cutpointr_eval, ...) {
    x <- cutpointr_eval$x
    class <- cutpointr_eval$class
    subgroup <- cutpointr_eval$subgroup
    method <- cutpointr_eval$method
    metric <- cutpointr_eval$metric
    pos_class <- cutpointr_eval$pos_class
    neg_class <- cutpointr_eval$neg_class
    direction <- cutpointr_eval$direction
    boot_runs <- cutpointr_eval$boot_runs
    use_midpoints <- cutpointr_eval$use_midpoints
    na.rm <- cutpointr_eval$na.rm
    allowParallel <- cutpointr_eval$allowParallel
    predictor <- cutpointr_eval$predictor
    outcome <- cutpointr_eval$outcome
    mod_name <- cutpointr_eval$mod_name
    subgroup_var <- cutpointr_eval$subgroup_var

    #
    # Prep
    #

    #NA
    if (any(anyNA(c(x, class)) | (!is.null(subgroup) & anyNA(subgroup))) &
         (!na.rm)) {
        stop("NAs found but na.rm = FALSE")
    }

    # Check number of classes
    if (na.rm) uc <- unique(stats::na.omit(class)) else uc <- unique(class)
    luc <- length(uc)
    if (luc != 2) stop(paste("Expecting two classes, got", luc))

    # Determine direction and/or pos_class if necessary:
    if (any(c(is.null(pos_class), is.null(neg_class), is.null(direction)))) {
        assumptions <- assume_direction_pos_class(x = x, class = class,
                                                  pos_class = pos_class,
                                                  neg_class = neg_class,
                                                  direction = direction,
                                                  na.rm = na.rm,
                                                  uc = uc)
    }
    if (is.null(direction)) direction <- assumptions$direction
    stopifnot(direction %in% c("<", ">", ">=", "<="))
    if (is.null(pos_class)) pos_class <- assumptions$pos_class
    if (is.null(neg_class)) neg_class <- assumptions$neg_class

    #
    # Calculate optimal cutpoint, map to cutpoint function
    #
    if (!is.null(subgroup)) {
        dat <- tibble::tibble_(list(x = ~ x, class = ~ class,
                                    subgroup = ~ subgroup))
        colnames(dat) <- c(predictor, outcome, "subgroup")
        if (na.rm) dat <- stats::na.omit(dat)
        g <- unique(dat$subgroup)
        dat <- dat %>%
            dplyr::group_by_("subgroup") %>%
            tidyr::nest_(key_col = "data") %>%
            dplyr::mutate_(subgroup = ~ as.character(subgroup),
                           pos_class = ~ pos_class,
                           prevalence = ~ purrr::map_dbl(data, function(g) {
                               mean(unlist(g[, outcome]) == pos_class)
                           })
            )
        optcut <- purrr::pmap_df(list(dat$subgroup, dat$data), function(g, d) {
            if (nrow(d) <= 1) stop(paste("Subgroup", g, "has <= 1 observations"))
            optcut <- tibble::tibble_(list(subgroup = ~ g))
            optcut <- dplyr::bind_cols(optcut,
                            method(data = d, x = predictor, class = outcome,
                                   metric_func = metric,
                                   direction = direction, pos_class = pos_class,
                                   neg_class = neg_class, ...))
            # If method is e.g. oc_OptimalCutpoints roc_curve is missing
            if (suppressWarnings(is.null(optcut$roc_curve))) {
                roc_curve <- roc(data = d, x = predictor, class = outcome,
                                 pos_class = pos_class, neg_class = neg_class,
                                 direction = direction)
                roc_curve <- tidyr::nest_(roc_curve, key_col = "roc_curve",
                                          nest_cols = colnames(roc_curve))
                optcut <- dplyr::bind_cols(roc_curve,
                                           tibble::as_tibble(optcut))
            }
            # If no metric is returned
            if (ncol(optcut) <= 3) {
                opt_ind <- get_opt_ind(optcut$roc_curve[[1]],
                                       oc = optcut$optimal_cutpoint,
                                       direction = direction)
                m <- metric(tp = optcut$roc_curve[[1]]$tp[opt_ind],
                            fp = optcut$roc_curve[[1]]$fp[opt_ind],
                            tn = optcut$roc_curve[[1]]$tn[opt_ind],
                            fn = optcut$roc_curve[[1]]$fn[opt_ind])
                optcut <- dplyr::bind_cols(optcut, tibble::as_tibble(m))
            }
            optcut$metric_name <- colnames(optcut)[4]
            sesp <- sesp_from_oc(optcut$roc_curve[[1]],
                                  oc = optcut$optimal_cutpoint,
                                  direction = direction)
            optcut$sensitivity <- sesp[, "Sensitivity"]
            optcut$specificity <- sesp[, "Specificity"]
            optcut$accuracy <- accuracy_from_oc(optcut$roc_curve[[1]],
                                                oc = optcut$optimal_cutpoint,
                                                direction = direction)[, "Accuracy"]
            if (use_midpoints) {
                optcut$optimal_cutpoint <- midpoint(oc = optcut$optimal_cutpoint,
                                                    x = unlist(d[, predictor],
                                                               use.names = FALSE),
                                                    direction = direction)
            }
            return(optcut)
        })
        optcut <- optcut %>%
            dplyr::mutate_(AUC = ~ purrr::map_dbl(roc_curve, function(r) {
                auc(tpr = r$tpr, fpr = r$fpr)
            }))
        optcut <- tibble::as_tibble(optcut) # can pmap_df return a tibble so this is not necessary?
        optcut <- dplyr::full_join(optcut, dat, by = "subgroup")
    } else {
        dat <- tibble::tibble_(list(x = ~ x, class = ~ class))
        colnames(dat) <- c(predictor, outcome)
        if (na.rm) dat <- stats::na.omit(dat)
        dat <- dat %>%
            tidyr::nest_(key_col = "data", nest_cols = colnames(dat)) %>%
            dplyr::mutate_(pos_class = ~ pos_class,
                           prevalence = ~ purrr::map_dbl(data, function(g) {
                               mean(unlist(g[, outcome]) == pos_class)
                           })
            )
        optcut <- purrr::map_df(dat$data, function(d) {
            optcut <- method(data = d,  x = predictor, class = outcome,
                             metric_func = metric,
                             direction = direction, pos_class = pos_class,
                             neg_class = neg_class, ...)
            if (suppressWarnings(is.null(optcut$roc_curve))) {
                roc_curve <- roc(data = d, x = predictor, class = outcome,
                                 pos_class = pos_class, neg_class = neg_class,
                                 direction = direction)
                roc_curve <- tidyr::nest_(roc_curve, key_col = "roc_curve",
                                          nest_cols = colnames(roc_curve))
                optcut <- dplyr::bind_cols(roc_curve,
                                           tibble::as_tibble(optcut))
            }
            # If no metric is returned
            if (ncol(optcut) <= 2) {
                opt_ind <- get_opt_ind(optcut$roc_curve[[1]],
                                       oc = optcut$optimal_cutpoint,
                                       direction = direction)
                m <- metric(tp = optcut$roc_curve[[1]]$tp[opt_ind],
                            fp = optcut$roc_curve[[1]]$fp[opt_ind],
                            tn = optcut$roc_curve[[1]]$tn[opt_ind],
                            fn = optcut$roc_curve[[1]]$fn[opt_ind])
                optcut <- dplyr::bind_cols(optcut, tibble::as_tibble(m))
            }
            optcut$metric_name <- colnames(optcut)[3]
            sesp <- sesp_from_oc(optcut$roc_curve[[1]],
                                  oc = optcut$optimal_cutpoint,
                                  direction = direction)
            optcut$sensitivity <- sesp[, "Sensitivity"]
            optcut$specificity <- sesp[, "Specificity"]
            optcut$accuracy <- accuracy_from_oc(optcut$roc_curve[[1]],
                                                oc = optcut$optimal_cutpoint,
                                                direction = direction)[, "Accuracy"]
            if (use_midpoints) {
                optcut$optimal_cutpoint <- midpoint(oc = optcut$optimal_cutpoint,
                                                    x = unlist(d[, predictor],
                                                               use.names = FALSE),
                                                    direction = direction)
            }
            return(optcut)
        })
        optcut <- optcut %>%
            dplyr::mutate_(AUC = ~ purrr::map_dbl(roc_curve, function(r) {
                auc(tpr = r$tpr, fpr = r$fpr)
            }))
        optcut <- tibble::as_tibble(optcut)
        optcut <- dplyr::bind_cols(optcut, dat)
    }


    optcut$direction                        <- direction
    optcut$predictor                        <- predictor
    optcut$outcome                          <- outcome
    optcut$neg_class                        <- neg_class
    optcut$method                           <- mod_name
    if (!is.null(subgroup)) optcut$grouping <- subgroup_var

    # Reorder for nicer output
    mn <- optcut$metric_name[[1]]
    check_colnames(metric_name = mn)
    select_cols <- c("subgroup", "direction", "optimal_cutpoint",
                     "method", mn,
                     "accuracy", "sensitivity", "specificity", "AUC",
                     "pos_class", "neg_class", "prevalence",
                     "outcome", "predictor", "grouping", "data", "roc_curve")
    # subgroup and grouping may not be given
    select_cols <- select_cols[select_cols %in% colnames(optcut)]
    optcut <- optcut[, select_cols]

    #
    # Bootstrap cutpoint variability and get LOO-Bootstrap performance estimate
    # Data are already nested and grouped if necessary
    #
    if (allowParallel) {
        requireNamespace("foreach")
        `%seq_or_par%` <- doRNG::`%dorng%`
    } else {
        `%seq_or_par%` <- foreach::`%do%`
    }
    if (boot_runs <= 0) {
        bootstrap <- NULL
    } else {
        boot_runs <- ceiling(boot_runs)
        bootstrap <- dat %>%
            dplyr::transmute_(boot = ~ purrr::map(data, function(g) {
                boot_g <- foreach::foreach(rep = 1:boot_runs, .combine = rbind,
                    .packages = "OptimalCutpoints",
                    .export = c("method", "direction", "pos_class", "metric",
                    "neg_class", "mn", "use_midpoints",
                    "predictor", "outcome")) %seq_or_par%
                    {
                        b_ind   <- sample(1:nrow(g), replace = T, size = nrow(g))
                        if (length(unique(unlist(g[b_ind, outcome]))) == 1) {
                            optcut_b <- data.frame(NA, NA)
                            colnames(optcut_b) <- c("optimal_cutpoint", mn)
                        } else {
                            optcut_b <- method(data = g[b_ind, ], x = predictor,
                                                metric_func = metric,
                                                class = outcome,
                                                direction = direction,
                                                pos_class = pos_class,
                                                neg_class = neg_class, ...)
                            if (use_midpoints) {
                                optcut_b$optimal_cutpoint <-
                                    midpoint(oc = optcut_b$optimal_cutpoint,
                                             x = unlist(g[b_ind, predictor],
                                                        use.names = FALSE),
                                             direction = direction)
                            }
                        }
                        # LOO-Bootstrap
                        if (suppressWarnings(is.null(optcut_b$roc_curve))) {
                            roc_curve_b <- roc(data = g[b_ind, ], x = predictor,
                                               class = outcome,
                                               pos_class = pos_class, neg_class = neg_class,
                                               direction = direction)
                            roc_curve_b <- tidyr::nest_(roc_curve_b, key_col = "roc_curve",
                                                        nest_cols = colnames(roc_curve_b))
                            optcut_b$roc_curve <- roc_curve_b[[1]]
                        }
                        opt_ind_b <- get_opt_ind(roc_curve = optcut_b$roc_curve[[1]],
                                                 oc = optcut_b$optimal_cutpoint,
                                                 direction = direction)
                        Sens_Spec_b <- sesp_from_oc(optcut_b$roc_curve[[1]],
                                                     oc = optcut_b$optimal_cutpoint,
                                                     direction = direction,
                                                     opt_ind = opt_ind_b)
                        Acc_b <- accuracy_from_oc(optcut_b$roc_curve[[1]],
                                                  oc = optcut_b$optimal_cutpoint,
                                                  direction = direction,
                                                  opt_ind = opt_ind_b)
                        kap_b <- kappa_from_oc(optcut_b$roc_curve[[1]],
                                               oc = optcut_b$optimal_cutpoint,
                                               direction = direction,
                                               opt_ind = opt_ind_b)
                        roc_curve_oob <- roc(data = g[-b_ind, ], x = predictor,
                                             class = outcome,
                                             pos_class = pos_class, neg_class = neg_class,
                                             direction = direction)
                        opt_ind_oob <- get_opt_ind(roc_curve = roc_curve_oob,
                                                 oc = optcut_b$optimal_cutpoint,
                                                 direction = direction)
                        Sens_Spec_oob <- sesp_from_oc(roc_curve_oob,
                                                     oc = optcut_b$optimal_cutpoint,
                                                     direction = direction,
                                                     opt_ind = opt_ind_oob)
                        Acc_oob <- accuracy_from_oc(roc_curve_oob,
                                                  oc = optcut_b$optimal_cutpoint,
                                                  direction = direction,
                                                  opt_ind = opt_ind_oob)
                        kap_oob <- kappa_from_oc(roc_curve_oob,
                                               oc = optcut_b$optimal_cutpoint,
                                               direction = direction,
                                               opt_ind = opt_ind_oob)
                        metric_oob <- metric(tp = roc_curve_oob$tp[opt_ind_oob],
                                             fp = roc_curve_oob$fp[opt_ind_oob],
                                             tn = roc_curve_oob$tn[opt_ind_oob],
                                             fn = roc_curve_oob$fn[opt_ind_oob])
                        bootstrap <- cbind(optimal_cutpoint = optcut_b$optimal_cutpoint,
                                           metric_oob,
                                           Accuracy_b       = as.numeric(Acc_b),
                                           Accuracy_oob     = as.numeric(Acc_oob),
                                           Sensitivity_b    = Sens_Spec_b[1],
                                           Sensitivity_oob  = Sens_Spec_oob[1],
                                           Specificity_b    = Sens_Spec_b[2],
                                           Specificity_oob  = Sens_Spec_oob[2],
                                           Kappa_b          = as.numeric(kap_b),
                                           Kappa_oob        = as.numeric(kap_oob),
                                           TP_b = optcut_b$roc_curve[[1]]$tp[opt_ind_b],
                                           FP_b = optcut_b$roc_curve[[1]]$fp[opt_ind_b],
                                           TN_b = optcut_b$roc_curve[[1]]$tn[opt_ind_b],
                                           FN_b = optcut_b$roc_curve[[1]]$fn[opt_ind_b],
                                           TP_oob = roc_curve_oob$tp[opt_ind_oob],
                                           FP_oob = roc_curve_oob$fp[opt_ind_oob],
                                           TN_oob = roc_curve_oob$tn[opt_ind_oob],
                                           FN_oob = roc_curve_oob$fn[opt_ind_oob]
                        )
                        bootstrap <- tibble::as_data_frame(bootstrap)
                        return(bootstrap)
                    }
                lna <- sum(is.na(boot_g))
                if (lna) warning(paste(lna, "Missing values in bootstrap, maybe due to sampling of only one class"))
                return(boot_g)
            }))
    }
    res <- dplyr::bind_cols(optcut, bootstrap)
    class(res) <- c("cutpointr", class(res))
    return(res)
}

