#' Determine and evaluate optimal cutpoints
#'
#' Using predictions (or e.g. biological marker values) and binary class labels, this function
#' will determine "optimal" cutpoints using various selectable methods. The
#' methods for cutpoint determination can be evaluated using bootstrapping. An
#' estimate of the cutpoint variability and the out-of-sample performance will then
#' be returned.
#'
#' If direction and/or pos_class and neg_class are not given, the function will
#' assume that higher values indicate the positive class and use the class
#' with a higher median as the positive class.
#'
#' Different methods can be selected for determining the optimal cutpoint via
#' the method argument. The package includes the following cutpoint functions:
#' \itemize{
#'  \item maximize_metric: Maximize the metric function
#'  \item minimize_metric: Minimize the metric function
#'  \item maximize_loess_metric: Maximize the metric function after LOESS
#'  smoothing
#'  \item minimize_loess_metric: Minimize the metric function after LOESS
#'  smoothing
#'  \item maximize_boot_metric: Maximize the metric function as a mean of
#'  the optimal cutpoints in bootstrapped samples
#'  \item minimize_boot_metric: Minimize the metric function as a mean of
#'  the optimal cutpoints in bootstrapped samples
#'  \item oc_manual: Specify the cutpoint manually
#'  \item oc_youden_kernel: Maximize the Youden-Index after kernel smoothing
#'  the distributions of the two classes
#'  \item oc_youden_normal: Maximize the Youden-Index parametrically
#'  assuming normally distributed data in both classes
#' }
#'
#' User-defined functions can be supplied to method, too. As a reference,
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
#'  \item ... Further arguments
#' }
#'
#' The ... argument can be used to avoid an error if not all of the above
#' arguments are needed or in oder to pass additional arguments to method.
#' The function should return a data.frame or tbl_df with
#' one row, the column "optimal_cutpoint", and an optional column with an arbitraty name
#' with the metric value at the optimal cutpoint.
#'
#' Built-in metric functions include:
#' \itemize{
#'  \item accuracy: Fraction correctly classified
#'  \item youden: Youden- or J-Index = sensitivity + specificity - 1
#'  \item sum_sens_spec: sensitivity + specificity
#'  \item sum_ppv_npv: The sum of positive predictive value (PPV) and negative
#'  predictive value (NPV)
#'  \item prod_sens_spec: sensitivity * specificity
#'  \item prod_ppv_npv: The product of positive predictive value (PPV) and
#'  negative predictive value (NPV)
#'  \item cohens_kappa: Cohen's Kappa
#'  \item abs_d_sens_spec: The absolute difference between
#'  sensitivity and specificity
#'  \item abs_d_ppv_npv: The absolute difference between positive predictive
#'  value (PPV) and negative predictive value (NPV)
#'  \item p_chisquared: The p-value of a chi-squared test on the confusion
#'  matrix of predictions and observations
#'  \item odds_ratio: The odds ratio calculated as (TP / FP) / (FN / TN)
#'  \item risk_ratio: The risk ratio (relative risk) calculated as
#'  (TP / (TP + FN)) / (FP / (FP + TN))
#'  \item misclassification_cost: The sum of the misclassification cost of
#'  false positives and false negatives. Additional arguments: cost_fp, cost_fn
#'  \item total_utility: The total utility of true / false positives / negatives
#'  calculated as utility_tp * TP + utility_tn * TN - cost_fp * FP - cost_fn * FN.
#'  Additional arguments: utility_tp, utility_tn, cost_fp, cost_fn
#'  \item F1_score: The F1-score (2 * TP) / (2 * TP + FP + FN)
#' }
#'
#' User defined metric functions can be used as well which can accept the following
#' inputs as vectors:
#' \itemize{
#'  \item tp: Vector of true positives
#'  \item fp: Vector of false positives
#'  \item tn: Vector of true negatives
#'  \item fn: Vector of false negatives
#'  \item ... If the metric function is used in conjunction with any of the
#'  maximize / minimize methods, further arguments can be passed
#' }
#'
#' The function should return a numeric vector or a matrix or a data.frame
#' with one column. If the column is named,
#' the name will be included in the output and plots. Avoid using names that
#' are identical to the column names that are by default returned by cutpointr.
#'
#' If boot_runs is positive, that number of bootstrap samples will be drawn
#' and the optimal cutpoint using method will be determined. Additionally,
#' as a way of validation, the function in metric will be used to
#' score the out-of-bag predictions using the cutpoints determined by
#' method. Various default metrics are always included in the bootstrap results.
#'
#' If multiple optimal cutpoints are found, the first one is returned and a
#' warning including all optimal cutpoints is issued. The first one refers to
#' the minimum of the optimal cutpoints if direction = ">=" or to the maximum
#' of the optimal cutpoints if direction = "<=".
#'
#' If use_midpoints = TRUE the mean of the optimal cutpoint and the next
#' highest or lowest possible cutpoint is returned, depending on direction.
#' If use_midpoints is set to TRUE and multiple optimal cutpoints are found,
#' the midpoint of the minimum / maximum of the optimal cutpoints
#' and the next highest / lowest observation is returned, as described before.
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
#'
#' \dontrun{
#' ## Predict class for new observations
#' predict(opt_cut, newdata = data.frame(dsi = 0:5))
#'
#' ## Supplying raw data, same result
#' cutpointr(x = suicide$dsi, class = suicide$suicide)
#'
#' ## direction, class labels, method and metric can be defined manually
#' ## Again, same result
#' cutpointr(suicide, dsi, suicide, direction = ">=", pos_class = "yes",
#'           method = maximize_metric, metric = youden)
#'
#' ## Optimal cutpoint for dsi, as before, but for the separate subgroups
#' opt_cut <- cutpointr(suicide, dsi, suicide, gender)
#' opt_cut
#'
#' ## Bootstrapping also works on individual subgroups
#' ## low boot_runs for illustrative purposes
#' set.seed(30)
#' opt_cut <- cutpointr(suicide, dsi, suicide, gender, boot_runs = 5)
#' opt_cut
#' summary(opt_cut)
#' plot(opt_cut)
#'
#' ## Transforming variables (unrealistic, just to show the functionality)
#' opt_cut <- cutpointr(suicide, x = log(dsi + 1), class = suicide == "yes",
#'     subgroup = dsi %% 2 == 0)
#' opt_cut
#' predict(opt_cut, newdata = data.frame(dsi = 1:3))
#'
#' ## Parallelized bootstrapping
#'   cl <- makeCluster(2) # 2 cores
#'   registerDoParallel(cl)
#'   registerDoRNG(12) # Reproducible parallel loops using doRNG
#'   opt_cut <- cutpointr(suicide, dsi, suicide, gender,
#'                        boot_runs = 10, allowParallel = TRUE)
#'   stopCluster(cl)
#'   opt_cut
#'   plot(opt_cut)
#'
#' ## Robust cutpoint method using kernel smoothing for optimizing Youden-Index
#' opt_cut <- cutpointr(suicide, dsi, suicide, gender,
#'                      method = oc_youden_kernel)
#' opt_cut
#' }
#'
#'
#'
#' @param data A data frame or tibble in which the columns that may be given in x,
#'  class and possibly subgroup can be found.
#' @param x The variable name without quotation marks to be used for
#'  classification, e.g. predictions or test values, or the raw data if data = NULL.
#' @param class The variable name without quotation marks indicating class membership
#' or the raw data if data = NULL.
#' @param subgroup The variable name without quotation marks
#' of an additional covariate that identifies subgroups or the raw data if
#' data = NULL. Separate optimal cutpoints will be determined by group.
#' Numeric, character and factor are allowed.
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
#' @param silent (logical) If TRUE suppresses all messages.
#' @param ... Further optional arguments that will be passed to method.
#' minimize_metric and maximize_metric pass ... to metric.
#' @importFrom purrr %>%
#' @importFrom foreach %do%
#' @return A cutpointr object which is also a data.frame and tbl_df.
#' @useDynLib cutpointr
#' @importFrom Rcpp sourceCpp
#' @export
cutpointr <- function(data = NULL, x, class, subgroup = NULL,
                      method = maximize_metric, metric = sum_sens_spec,
                      pos_class = NULL, neg_class = NULL, direction = NULL,
                      boot_runs = 0, use_midpoints = FALSE, na.rm = FALSE,
                      allowParallel = FALSE, silent = FALSE, ...) {

    #
    # NSE
    #
    if (is.null(data)) {
        predictor <- "x"
        outcome <- "class"
        subgroup_var <- "subgroup"
    } else {
        predictor <- deparse(substitute(x))
        outcome   <- deparse(substitute(class))
        x <- tryCatch(eval(substitute(x), data, parent.frame()),
                      error = function(cond) {
                          stop(paste("If the data argument is specified,",
                                     "x and class should refer to columns",
                                     "in data."))
                      })
        class <- tryCatch(eval(substitute(class), data, parent.frame()),
                          error = function(cond) {
                              stop(paste("If the data argument is specified,",
                                         "x and class should refer to columns",
                                         "in data."))
                          })
        subgroup_var <- deparse(substitute(subgroup))
        subgroup <- tryCatch(eval(substitute(subgroup), data, parent.frame()),
                             error = function(cond) {
                                 stop(paste("If the data argument is specified,",
                                            "subgroup should refer to a column",
                                            "in data."))
                             })
    }

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
        stop("metric should be a function")
    }
    cl <- match.call()
    metric_name <- cl$metric
    # if default was not changed:
    metric_name <- as.character(substitute(metric))
    if (is.null(metric_name)) stop("Could not get the metric function")
    if (silent) {
        suppressMessages(
            cutpointr_internal(x, class, subgroup, method, metric, pos_class, neg_class,
                               direction, boot_runs, use_midpoints, na.rm, allowParallel,
                               predictor, outcome, mod_name, subgroup_var, ...)
        )
    } else {
        cutpointr_internal(x, class, subgroup, method, metric, pos_class, neg_class,
                           direction, boot_runs, use_midpoints, na.rm, allowParallel,
                           predictor, outcome, mod_name, subgroup_var, ...)
    }
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
#' @examples
#' library(cutpointr)
#'
#' ## Optimal cutpoint for dsi
#' data(suicide)
#' opt_cut <- cutpointr_(suicide, "dsi", "suicide")
#' opt_cut
#' summary(opt_cut)
#' plot(opt_cut)
#' predict(opt_cut, newdata = data.frame(dsi = 0:5))
cutpointr_ <- function(data, x, class, subgroup = NULL,
                      method = maximize_metric, metric = sum_sens_spec,
                      pos_class = NULL, neg_class = NULL, direction = NULL,
                      boot_runs = 0, use_midpoints = FALSE, na.rm = FALSE,
                      allowParallel = FALSE, silent = FALSE, ...) {
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
        stop("metric should be a function")
    }
    cl <- match.call()
    metric_name <- cl$metric
    # if default was not changed:
    metric_name <- as.character(substitute(metric))
    if (is.null(metric_name)) stop("Could not get the metric function")

    if (silent) {
        suppressMessages(
            cutpointr_internal(x, class, subgroup, method, metric, pos_class, neg_class,
                               direction, boot_runs, use_midpoints, na.rm, allowParallel,
                               predictor, outcome, mod_name, subgroup_var, ...)
        )
    } else {
        cutpointr_internal(x, class, subgroup, method, metric, pos_class, neg_class,
                           direction, boot_runs, use_midpoints, na.rm, allowParallel,
                           predictor, outcome, mod_name, subgroup_var, ...)
    }
}


cutpointr_internal <- function(x, class, subgroup, method, metric, pos_class,
                               neg_class, direction, boot_runs,
                               use_midpoints, na.rm, allowParallel, predictor,
                               outcome, mod_name, subgroup_var, ...) {
    #
    # Prep
    #

    #NA
    if (any(anyNA(c(x, class)) | (!is.null(subgroup) & anyNA(subgroup))) &
         (!na.rm)) {
        stop("NAs found but na.rm = FALSE")
    }

    # Check classes
    if (na.rm) uc <- unique(stats::na.omit(class)) else uc <- unique(class)
    luc <- length(uc)
    if (luc != 2) stop(paste("Expecting two classes, got", luc))
    if (!(is.null(pos_class))) {
        if (!(pos_class %in% class)) stop("pos_class not found in data")
    }
    if (!(is.null(neg_class))) {
        if (!(neg_class %in% class)) stop("neg_class not found in data")
    }

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
        dat <- data.frame(x = x, class = class, subgroup = subgroup,
                          stringsAsFactors = FALSE)
        colnames(dat) <- c(predictor, outcome, "subgroup")
        if (na.rm) dat <- stats::na.omit(dat)
        g <- unique(dat$subgroup)
        dat <- dat %>%
            dplyr::group_by_("subgroup") %>%
            tidyr::nest_(key_col = "data") %>%
            dplyr::mutate_(subgroup = ~ as.character(subgroup))
        dat$pos_class <- pos_class
        optcut <- purrr::pmap_df(list(dat$subgroup, dat$data), function(g, d) {
            if (nrow(d) <= 1) stop(paste("Subgroup", g, "has <= 1 observations"))
            optcut <- data.frame(subgroup = g, stringsAsFactors = FALSE)
            method_result <- method(data = d, x = predictor, class = outcome,
                                    metric_func = metric,
                                    direction = direction, pos_class = pos_class,
                                    neg_class = neg_class, ...)
            method_result <- check_method_cols(method_result)
            optcut <- dplyr::bind_cols(optcut, method_result)
            # Depending on method the roc_curve may be missing
            if (suppressWarnings(is.null(optcut$roc_curve))) {
                roc_curve <- roc(data = d, x = predictor, class = outcome,
                                 pos_class = pos_class, neg_class = neg_class,
                                 direction = direction)
                roc_curve <- tidyr::nest_(roc_curve, key_col = "roc_curve")
                optcut <- dplyr::bind_cols(roc_curve,
                                           tibble::as_tibble(optcut))
            } else {
                check_roc_curve(optcut)
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
                m <- check_metric_name(m)
                colnames(m) <- make.names(colnames(m))
                optcut <- dplyr::bind_cols(optcut, tibble::as_tibble(m))
            }
            optcut <- check_colnames(optcut)
            sesp <- sesp_from_oc(optcut$roc_curve[[1]],
                                  oc = optcut$optimal_cutpoint,
                                  direction = direction)
            optcut$sensitivity <- sesp[, "sensitivity"]
            optcut$specificity <- sesp[, "specificity"]
            optcut$acc <- accuracy_from_oc(optcut$roc_curve[[1]],
                                           oc = optcut$optimal_cutpoint,
                                           direction = direction)[, "accuracy"]
            if (use_midpoints) {
                optcut$optimal_cutpoint <- midpoint(oc = optcut$optimal_cutpoint,
                                                    x = unlist(d[, predictor],
                                                               use.names = FALSE),
                                                    direction = direction)
            }
            return(optcut)
        })
        optcut <- optcut %>%
            dplyr::mutate_(
                AUC = ~ purrr::map_dbl(roc_curve, function(r) {
                    auc(tpr = r$tpr, fpr = r$fpr)
                }),
                prevalence = ~ purrr::map_dbl(roc_curve, function(r) {
                    utils::tail(r$tp, 1) / (utils::tail(r$tp, 1) + utils::tail(r$fp, 1))
                })
            )
        optcut <- tibble::as_tibble(optcut)
        optcut <- dplyr::full_join(optcut, dat, by = "subgroup")
    } else {
        dat <- tibble::tibble_(list(x = ~ x, class = ~ class))
        colnames(dat) <- c(predictor, outcome)
        if (na.rm) dat <- stats::na.omit(dat)
        dat <- dat %>%
            tidyr::nest_(key_col = "data")
        dat$pos_class <- pos_class
        optcut <- method(data = dat$data[[1]],  x = predictor, class = outcome,
                         metric_func = metric,
                         direction = direction, pos_class = pos_class,
                         neg_class = neg_class, ...)
        optcut <- check_method_cols(optcut)
        if (suppressWarnings(is.null(optcut$roc_curve))) {
            roc_curve <- roc(data = dat$data[[1]],
                             x = predictor, class = outcome,
                             pos_class = pos_class, neg_class = neg_class,
                             direction = direction)
            roc_curve <- tidyr::nest_(roc_curve, key_col = "roc_curve",
                                      nest_cols = colnames(roc_curve))
            optcut <- dplyr::bind_cols(roc_curve,
                                       tibble::as_tibble(optcut))
        } else {
            check_roc_curve(optcut)
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
            m <- check_metric_name(m)
            colnames(m) <- make.names(colnames(m))
            optcut <- dplyr::bind_cols(optcut, tibble::as_tibble(m))
        }
        optcut <- check_colnames(optcut)
        sesp <- sesp_from_oc(optcut$roc_curve[[1]],
                             oc = optcut$optimal_cutpoint,
                             direction = direction)
        optcut$sensitivity <- sesp[, "sensitivity"]
        optcut$specificity <- sesp[, "specificity"]
        optcut$acc <- accuracy_from_oc(optcut$roc_curve[[1]],
                                       oc = optcut$optimal_cutpoint,
                                       direction = direction)[, "accuracy"]
        if (use_midpoints) {
            optcut$optimal_cutpoint <- midpoint(oc = optcut$optimal_cutpoint,
                                                x = unlist(dat$data[[1]][, predictor],
                                                           use.names = FALSE),
                                                direction = direction)
        }
        optcut$AUC <- auc(tpr = optcut$roc_curve[[1]]$tpr,
                          fpr = optcut$roc_curve[[1]]$fpr)
        optcut$prevalence <- utils::tail(optcut$roc_curve[[1]]$tp, 1) /
            (utils::tail(optcut$roc_curve[[1]]$tp, 1) +
                 utils::tail(optcut$roc_curve[[1]]$fp, 1))
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
    mn <- optcut$metric_name[1]
    select_cols <- c("subgroup", "direction", "optimal_cutpoint",
                     "method", mn,
                     "acc", "sensitivity", "specificity", "AUC",
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
        message("Running bootstrap...")
        boot_runs <- ceiling(boot_runs)
        bootstrap <- dat %>%
             dplyr::transmute_(boot = ~ purrr::map2(dat$data, dat$pos_class,
                                                    function(g, pc) {
                boot_g <- foreach::foreach(rep = 1:boot_runs, .combine = rbind,
                    .export = c("method", "direction", "metric",
                    "neg_class", "mn", "use_midpoints",
                    "predictor", "outcome")) %seq_or_par%
                    {
                        b_ind <- sample(1:nrow(g), replace = TRUE, size = nrow(g))
                        if (only_one_unique(unlist(g[b_ind, outcome]))) {
                            optcut_b <- data.frame(NA, NA)
                            colnames(optcut_b) <- c("optimal_cutpoint", mn)
                        } else {
                            optcut_b <- method(data = g[b_ind, ], x = predictor,
                                                metric_func = metric,
                                                class = outcome,
                                                direction = direction,
                                                pos_class = pc,
                                                neg_class = neg_class, ...)
                            optcut_b <- check_method_cols(optcut_b)
                            optcut_b <- tibble::as_tibble(optcut_b)
                            if (use_midpoints) {
                                optcut_b$optimal_cutpoint <- midpoint(
                                    oc = optcut_b$optimal_cutpoint,
                                    x = unlist(g[b_ind, predictor],
                                               use.names = FALSE),
                                    direction = direction)
                            }
                        }
                        # LOO-Bootstrap
                        if (suppressWarnings(is.null(optcut_b$roc_curve))) {
                            roc_curve_b <- roc(data = g[b_ind, ], x = predictor,
                                               class = outcome,
                                               pos_class = pc, neg_class = neg_class,
                                               direction = direction, silent = TRUE)
                            roc_curve_b <- tidyr::nest_(roc_curve_b,
                                                        key_col = "roc_curve")
                            optcut_b <- dplyr::bind_cols(optcut_b, roc_curve_b)
                        }
                        opt_ind_b <- get_opt_ind(roc_curve = optcut_b$roc_curve[[1]],
                                                 oc = optcut_b$optimal_cutpoint,
                                                 direction = direction)
                        auc_b <- auc(tpr = optcut_b$roc_curve[[1]]$tpr,
                                     fpr = optcut_b$roc_curve[[1]]$fpr)
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
                        metric_b <- metric(tp = optcut_b$roc_curve[[1]]$tp[opt_ind_b],
                                             fp = optcut_b$roc_curve[[1]]$fp[opt_ind_b],
                                             tn = optcut_b$roc_curve[[1]]$tn[opt_ind_b],
                                             fn = optcut_b$roc_curve[[1]]$fn[opt_ind_b])
                        metric_b <- check_metric_name(metric_b)
                        roc_curve_oob <- roc(data = g[-b_ind, ], x = predictor,
                                             class = outcome,
                                             pos_class = pc, neg_class = neg_class,
                                             direction = direction, silent = TRUE)
                        opt_ind_oob <- get_opt_ind(roc_curve = roc_curve_oob,
                                                 oc = optcut_b$optimal_cutpoint,
                                                 direction = direction)
                        auc_oob <- auc(tpr = roc_curve_oob$tpr,
                                       fpr = roc_curve_oob$fpr)
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
                        metric_oob <- check_metric_name(metric_oob)
                        mn <- make.names(colnames(metric_oob))

                        bootstrap <- tibble::tibble(
                            optimal_cutpoint =  optcut_b$optimal_cutpoint,
                            AUC_b             =  auc_b,
                            AUC_oob           =  auc_oob,
                            metric_b          =  as.numeric(metric_b),
                            metric_oob        =  as.numeric(metric_oob),
                            acc_b       =  as.numeric(Acc_b),
                            acc_oob     =  as.numeric(Acc_oob),
                            sensitivity_b    =  Sens_Spec_b[1],
                            sensitivity_oob  =  Sens_Spec_oob[1],
                            specificity_b    =  Sens_Spec_b[2],
                            specificity_oob  =  Sens_Spec_oob[2],
                            kappa_b          =  as.numeric(kap_b),
                            kappa_oob        =  as.numeric(kap_oob),
                            TP_b =  optcut_b$roc_curve[[1]]$tp[opt_ind_b],
                            FP_b =  optcut_b$roc_curve[[1]]$fp[opt_ind_b],
                            TN_b =  optcut_b$roc_curve[[1]]$tn[opt_ind_b],
                            FN_b =  optcut_b$roc_curve[[1]]$fn[opt_ind_b],
                            TP_oob =  roc_curve_oob$tp[opt_ind_oob],
                            FP_oob =  roc_curve_oob$fp[opt_ind_oob],
                            TN_oob =  roc_curve_oob$tn[opt_ind_oob],
                            FN_oob =  roc_curve_oob$fn[opt_ind_oob],
                            roc_curve_b =  optcut_b$roc_curve
                        )
                        colnames(bootstrap)[4:5] <- paste0(mn, c("_b", "_oob"))
                        roc_curve_oob <- tidyr::nest_(roc_curve_oob,
                                                      key_col = "roc_curve_oob")
                        bootstrap <- dplyr::bind_cols(bootstrap, roc_curve_oob)
                        return(bootstrap)
                    }
                lna <- sum(is.na(boot_g))
                if (lna) warning(paste(lna, "Missing values in bootstrap, maybe",
                                       "due to sampling of only one class"))
                return(boot_g)
            }))
    }
    res <- dplyr::bind_cols(optcut, bootstrap)
    class(res) <- c("cutpointr", class(res))
    return(res)
}


#' Calculate optimal cutpoints and further statistics for multiple predictors
#'
#' Runs cutpointr_ over multiple predictor variables. By default, cutpointr
#' will be run using all columns in the data set as predictors except for the
#' variable in class.
#'
#' The automatic determination of positive / negative classes and direction
#' will be carried out separately for every predictor variable. That way, if
#' direction and the classes are not specified, the reported AUC for every
#' variable will be >= 0.5. AUC may be < 0.5 in the case of subgroups as
#' direction is equal in every subgroup.
#'
#' @param data A data frame.
#' @param x Character vector of predictor variables.
#' @param class The name of the outcome / independent variable.
#' @param silent Whether to display messages.
#' @param ... Further arguements to be passed to cutpointr.
#' @examples
#' library(cutpointr)
#'
#' multi_cutpointr(suicide, x = c("age", "dsi"), class = "suicide",
#'                 pos_class = "yes")
#'
#' multi_cutpointr(suicide, x = c("age", "dsi"), class = "suicide",
#'                 subgroup = "gender", pos_class = "yes")
#'
#' @return A data frame.
#' @importFrom purrr %>%
#' @export
multi_cutpointr <- function(data, x = colnames(data)[colnames(data) != class],
                            class, silent = FALSE, ...) {
    if(!(is.character(class) & length(class == 1))) {
        stop("class should be the name of the outcome variable (character)")
    }
    args <- list(...)
    if ("subgroup" %in% names(args)) {
        x <- x[x != args$subgroup]
    }
    res <- purrr::map_df(x, function(coln) {
        if (!silent) message(paste0(coln, ":"))
        cutpointr_(data, coln, class, silent = silent, ...) %>%
            dplyr::mutate(variable = coln)
    })
    res <- res[, c("variable", colnames(res)[colnames(res) != "variable"])]
    class(res) <- c("multi_cutpointr", class(res))
    return(res)
}