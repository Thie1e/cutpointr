#' Determine and evaluate optimal cutpoints.
#'
#' Using predictions (e.g. test values) and binary class labels, this function
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
#' the optcut_func argument. The package includes the following cutpoint functions:
#' \itemize{
#'  \item oc_youden: Youden- or J-Index, maximize sensitivity + specificity - 1
#'  \item oc_equalsesp: minimize the absolute difference of sensitivity and specificity
#'  \item oc_OptimalCutpoints: A wrapper for optimal.cutpoints from the OptimalCutpoints package.
#'  Supply an additional "methods" argument with the method choice corresponding
#'  to a method from the OptimalCutpoints package
#' }
#'
#' User defined functions can be supplied to optcut_func, too. As a reference,
#' the code of all included cutpoint functions can be accessed by simply typing
#' their name. To define a new cutpoint function, create a function that may take
#' as input(s):
#' \itemize{
#'  \item data
#'  \item x
#'  \item class
#'  \item pos_class
#'  \item neg_class
#'  \item direction
#' }
#'
#' The ... argument can be used to avoid an error if not all of the above
#' arguments are needed. The function should return a data frame or tbl_df with
#' one row, the column "optimal_cutpoint, and a column with an arbitraty name
#' with the metric value at the optimal cutpoint.
#'
#' If boot_runs is positive, that number of bootstrap samples will be drawn
#' and the optimal cutpoint using optcut_func will be determined. Additionally,
#' as an alternative to cross validation, boot_metric_func will be used to
#' score the out-of-bag predictions using the cutpoints determined by
#' optcut_func. By default, out-of-bag accuracy will be assessed. Accuracy,
#' Sensitivity, Specificity, Kappa, true positives/negatives and false
#' positives/negatives are always included in the bootstrap results.
#' User defined functions can be used as well which can accept the following
#' inputs:
#' \itemize{
#'  \item tp
#'  \item fp
#'  \item tn
#'  \item fn
#' }
#'
#' If not all inputs are needed ... can be used to avoid an unused argument error.
#'
#' If multiple optimal cutpoints are found, the first one is returned and a
#' warning including all optimal cutpoints is issued. The first one refers to
#' the minimum of the optimal cutpoints if direction = ">" or to the maximum
#' of the optimal cutpoints if direction = "<".
#'
#' If use_midpoints is set to TRUE and multiple optimal cutpoints are found,
#' the midpoint of the minimum / maximum of the optimal cutpoints, as described
#' above, and the next highest / lowest observation is returned. Thus, finding
#' multiple optimal cutpoints has no effect on determining the midpoint.
#'
#' @examples
#' library(cutpointr)
#' library(OptimalCutpoints)
#' data(elas)
#'
#' ## Optimal cutpoint for elas
#' opt_cut <- cutpointr(elas, elas, status)
#' opt_cut
#' plot(opt_cut)
#'
#' opt_cut <- cutpointr(elas, elas, status, direction = "<=", pos_class = 0,
#'                      method = maximize_metric, metric = youden)
#' opt_cut
#' plot(opt_cut)
#'
#' ## Optimal cutpoint for elas, as before, but for the separate subgroups
#' opt_cut <- cutpointr(elas, elas, status, gender)
#' opt_cut
#' plot(opt_cut)
#'
#' ## Bootstrapping to assess cutpoint variability and out-of-sample performance
#' set.seed(123)
#' opt_cut <- cutpointr(elas, elas, status, boot_runs = 200)
#' opt_cut
#' plot(opt_cut)
#'
#' ## Bootstrapping also works on individual subgroups
#' set.seed(123)
#' opt_cut <- cutpointr(elas, elas, status, gender, boot_runs = 200)
#' opt_cut
#' plot(opt_cut)
#'
#' ## Transforming variables (unrealistic, just to show the functionality)
#' set.seed(123)
#' opt_cut <- cutpointr(elas, log(elas), status == 1,
#'     gender == "Male" & elas %% 1 == 0, boot_runs = 200)
#' opt_cut
#' plot(opt_cut)
#'
#' ## Different cutpoint function / metric
#' set.seed(123)
#' opt_cut <- cutpointr(elas, elas, status, gender, pos_class = 1, boot_runs = 200,
#'                      method = minimize_metric, metric = abs_d_sesp)
#' opt_cut
#' plot(opt_cut)
#'
#' ## With NAs
#' elas_na <- elas
#' elas_na$elas[10] <- NA
#' elas_na$status[20] <- NA
#' elas_na$gender[30] <- NA
#' opt_cut_na <- cutpointr(elas_na, elas, status, gender, na.rm = TRUE)
#' opt_cut_na
#' plot(opt_cut_na)
#'
#' ## Parallelized bootstrapping
#' library(doSNOW)
#' cl <- makeCluster(2) # 2 cores
#' registerDoSNOW(cl)
#' library(doRNG)
#' registerDoRNG(123) # Reproducible parallel loops using doRNG
#' opt_cut <- cutpointr(elas, elas, status, gender, pos_class = 1,
#'                boot_runs = 2000, allowParallel = TRUE)
#' opt_cut
#' plot(opt_cut)
#'
#'
#' ## Wrapper for optimal.cutpoints
#' registerDoRNG(123) # Reproducible parallel loops using doRNG
#' opt_cut <- cutpointr(elas, elas, status, gender, pos_class = 1, boot_runs = 2000,
#'                      method = oc_OptimalCutpoints, methods = "Youden",
#'                      allowParallel = TRUE)
#' opt_cut
#' plot(opt_cut)
#'
#'
#' @param data A data frame or tibble in which the columns that are given in x, class and possibly subgroup can be found
#' @param x The variable name (with or without quotation marks) to be used for classification, e.g. predictions or test values.
#' @param class The variable name (with or without quotation marks) indicating class membership.
#' @param subgroup The variable name of an additional covariate that identifies subgroups. Separate
#' optimal cutpoints will be determined by group. Numeric, character and factor are
#' allowed. Also expressions like z > 10 are possible.
#' @param pos_class (optional) The value of class that indicates the positive class
#' @param neg_class (optional) The value of class that indicates the negative class
#' @param direction (character, optional) Use ">" or "<" to indicate whether x
#' is supposed to be larger or smaller for the positive class.
#' @param optcut_func (function or character) A function for determining cutpoints. Can
#' be user supplied or use some of the built in methods. See details.
#' @param boot_runs (numeric, optional) If positive, this number of bootstrap samples
#' will be used to assess the variability and the out-of-sample performance.
#' @param boot_metric_func (function) The function to compute a metric using the
#' out-of-bag values during bootstrapping. A way of validating the performance.
#' User defined functions can be supplied, see details.
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
cutpointr <- function(data, x, class, subgroup, pos_class = NULL,
                      neg_class = NULL, direction = NULL,
                      method = maximize_metric,
                      boot_runs = 0, metric = youden,
                      use_midpoints = FALSE, na.rm = FALSE,
                      allowParallel = FALSE, ...) {
    #
    # NSE
    #
    if (is.character(substitute(x))) x <- as.name(x)
    if (is.character(substitute(class))) class <- as.name(class)
    if (!missing(subgroup) && is.character(substitute(subgroup))) subgroup <- as.name(subgroup)
    predictor <- deparse(substitute(x))
    outcome   <- deparse(substitute(class))
    x <- eval(substitute(x), data, parent.frame())
    class <- eval(substitute(class), data, parent.frame())
    if (!missing(subgroup)) {
        subgroup_var <- deparse(substitute(subgroup))
        subgroup <- eval(substitute(subgroup), data, parent.frame())
    }

    # Get cutpoint function
    if (length(method) > 1 | !(class(method) %in% c("character", "function"))) {
        stop("method should be character string or a function")
    }
    if (is.character(method)) {
        # If a character vec is given the user surely wants to search in the package
        mod_names <- method[1]
        method <- paste0("cutpointr::", method)
        # method <- lapply(method, function(fun) eval(parse(text = fun)))
        method <- eval(parse(text = method))
    } else {
        cl <- match.call()
        mod_names <- cl$method
        # if default was not changed:
        mod_names <- as.character(substitute(method))
        mod_names <- mod_names[1]
    }
    if (is.null(mod_names)) stop("Could not get the names of the method function")
    stopifnot(is.function(metric))
    metric_name <- as.character(substitute(metric))

    #
    # Prep
    #

    #NA
    if (any(anyNA(c(x, class)) | (!missing(subgroup) && anyNA(subgroup))) &&
         (missing(na.rm) | !na.rm)) {
        stop("NAs found but na.rm = FALSE")
    }

    # Determine direction and/or pos_class if necessary:
    if (any(c(is.null(pos_class), is.null(neg_class), is.null(direction)))) {
        assumptions <- assume_direction_pos_class(x = x, class = class,
                                                  pos_class = pos_class,
                                                  neg_class = neg_class,
                                                  direction = direction,
                                                  na.rm = na.rm)
    }
    if (is.null(direction)) direction <- assumptions$direction
    if (is.null(pos_class)) pos_class <- assumptions$pos_class
    if (is.null(neg_class)) neg_class <- assumptions$neg_class

    #
    # Calculate optimal cutpoint, map to cutpoint function
    #
    if (!missing(subgroup)) {
        dat <- tibble::tibble(x, class, subgroup)
        if (na.rm) dat <- stats::na.omit(dat)
        g <- unique(dat$subgroup)
        dat <- dat %>%
            dplyr::group_by_("subgroup") %>%
            tidyr::nest_(data = ., key_col = "data", nest_cols = colnames(.)) %>%
            dplyr::mutate_(subgroup = ~ as.character(subgroup),
                           pos_class = ~ pos_class,
                           prevalence = ~ purrr::map_dbl(data, function(g) {
                               mean(g$class == pos_class)
                           })
            )
        optcut <- purrr::pmap_df(list(dat$subgroup, dat$data), function(g, d) {
            optcut <- method(data = d, x = "x", class = "class",
                             metric_func = metric,
                             direction = direction, pos_class = pos_class,
                             neg_class = neg_class, ...) %>%
                dplyr::mutate_(subgroup = ~ g)
            # If method is e.g. oc_OptimalCutpoints roc_curve is missing
            if (suppressWarnings(is.null(optcut$roc_curve))) {
                roc_curve <- roc(data = d, x = "x", class = "class",
                                 pos_class = pos_class, neg_class = neg_class,
                                 direction = direction)
                roc_curve <- tidyr::nest_(roc_curve, key_col = "roc_curve",
                                          nest_cols = colnames(roc_curve))
                optcut$roc_curve <- roc_curve[[1]]
            }
            sesp <- sesp_from_oc(x = d$x, class = d$class,
                                 oc = optcut$optimal_cutpoint,
                                 direction = direction, pos_class = pos_class,
                                 neg_class = neg_class)
            optcut$sensitivity <- sesp[, "Sensitivity"]
            optcut$specificity <- sesp[, "Specificity"]
            sesp <- sesp_from_oc2(optcut$roc_curve[[1]],
                                  oc = optcut$optimal_cutpoint,
                                  direction = direction)
            optcut$sensitivity2 <- sesp[, "Sensitivity"]
            optcut$specificity2 <- sesp[, "Specificity"]
            if (use_midpoints) {
                optcut$optimal_cutpoint <- midpoint(oc = optcut$optimal_cutpoint,
                                                    x = d$x, direction = direction)
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
        dat <- tibble::tibble(x, class)
        if (na.rm) dat <- stats::na.omit(dat)
        dat <- dat %>%
            tidyr::nest_(data = ., key_col = "data", nest_cols = colnames(.)) %>%
            dplyr::mutate_(pos_class = ~ pos_class,
                           prevalence = ~ purrr::map_dbl(data, function(g) {
                               mean(g$class == pos_class)
                           })
            )
        optcut <- purrr::map_df(dat$data, function(d) {
            optcut <- method(data = d,  x = "x", class = "class",
                             metric_func = metric,
                             direction = direction, pos_class = pos_class,
                             neg_class = neg_class, ...)
            if (suppressWarnings(is.null(optcut$roc_curve))) {
                roc_curve <- roc(data = d, x = "x", class = "class",
                                 pos_class = pos_class, neg_class = neg_class,
                                 direction = direction)
                roc_curve <- tidyr::nest_(roc_curve, key_col = "roc_curve",
                                          nest_cols = colnames(roc_curve))
                optcut$roc_curve <- roc_curve[[1]]
            }
            sesp <- sesp_from_oc(optcut$roc_curve[[1]],
                                  oc = optcut$optimal_cutpoint,
                                  direction = direction)
            optcut$sensitivity <- sesp[, "Sensitivity"]
            optcut$specificity <- sesp[, "Specificity"]
            if (use_midpoints) {
                optcut$optimal_cutpoint <- midpoint(oc = optcut$optimal_cutpoint,
                                                    x = d$x, direction = direction)
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
    optcut$method                           <- mod_names
    # optcut$metric                           <- metric_name
    if (!missing(subgroup)) optcut$grouping <- subgroup_var

    # Reorder for nicer output
    mn <- find_metric_name(colnames(optcut))
    select_cols <- c("subgroup", "direction", "optimal_cutpoint",
                     "sensitivity", "specificity", "sensitivity2", "specificity2",
                     "method", mn,
                     "pos_class", "neg_class", "prevalence", "AUC",
                     "outcome", "predictor", "grouping", "data", "roc_curve")
    # subgroup and grouping may not be given
    select_cols <- select_cols[select_cols %in% colnames(optcut)]
    optcut <- optcut[, select_cols]

    #
    # Bootstrap cutpoint variability and get LOO-Bootstrap performance estimate
    # Data are already nested and grouped if necessary
    #
    if (allowParallel) {
        require(doRNG)
        `%seq_or_par%` <- doRNG::`%dorng%`
    } else {
        `%seq_or_par%` <- `%do%`
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
                    "neg_class", "mn", "use_midpoints")) %seq_or_par%
                    {
                        b_ind   <- sample(1:nrow(g), replace = T, size = nrow(g))
                        if (length(unique(g[b_ind, ]$class)) == 1) {
                            funcout_b <- data.frame(NA, NA)
                            colnames(funcout_b) <- c("optimal_cutpoint", mn)
                            optcut_b <- NA
                        } else {
                            funcout_b <- method(data = g[b_ind, ], x = "x",
                                                metric_func = metric,
                                                class = "class",
                                                direction = direction,
                                                pos_class = pos_class,
                                                neg_class = neg_class, ...)
                            optcut_b  <- extract_opt_cut(funcout_b)
                            if (use_midpoints) {
                                optcut_b <- midpoint(oc = optcut_b,
                                                     x = g[b_ind, ]$x,
                                                     direction = direction)
                            }
                        }
                        # LOO-Bootstrap
                        preds_b <- ifel_pos_neg(g$x[b_ind] > optcut_b, pos_class, neg_class)
                        cm_b <- conf_mat(obs = g$class[b_ind],  preds = preds_b,
                                                pos_class = pos_class, neg_class = neg_class)
                        names(cm_b) <- paste0(names(cm_b), "_b")
                        Sens_Spec_b <- sens_spec(tp = cm_b["TP_b"], fp = cm_b["FP_b"],
                                                tn = cm_b["TN_b"], fn = cm_b["FN_b"])
                        Acc_b <- accuracy(tp = cm_b["TP_b"], fp = cm_b["FP_b"],
                                          tn = cm_b["TN_b"], fn = cm_b["FN_b"])
                        kap_b <- kappa(tp = cm_b["TP_b"], fp = cm_b["FP_b"],
                                            fn = cm_b["FN_b"], tn = cm_b["TN_b"])
                        preds_oob <- ifel_pos_neg(g$x[-b_ind] > optcut_b, pos_class, neg_class)
                        cm_oob <- conf_mat(obs = g$class[-b_ind],  preds = preds_oob,
                                           pos_class = pos_class, neg_class = neg_class)
                        names(cm_oob) <- paste0(names(cm_oob), "_oob")
                        Sens_Spec_oob <- sens_spec(tp = cm_oob["TP_oob"],
                                                   fp = cm_oob["FP_oob"],
                                                   tn = cm_oob["TN_oob"],
                                                   fn = cm_oob["FN_oob"])
                        Acc_oob <- accuracy(tp = cm_oob["TP_oob"],
                                            fp = cm_oob["FP_oob"],
                                            tn = cm_oob["TN_oob"],
                                            fn = cm_oob["FN_oob"])
                        kap_oob <- kappa(tp = cm_oob["TP_oob"],
                                            fp = cm_oob["FP_oob"],
                                            fn = cm_oob["FN_oob"],
                                            tn = cm_oob["TN_oob"])
                        metric_oob <- metric(tp = cm_oob["TP_oob"],
                                             fp = cm_oob["FP_oob"],
                                             tn = cm_oob["TN_oob"],
                                             fn = cm_oob["FN_oob"])
                        bootstrap <- cbind(optimal_cutpoint = optcut_b,
                                           metric_oob,
                                           Accuracy_b       = Acc_b,
                                           Accuracy_oob     = Acc_oob,
                                           Sensitivity_b    = Sens_Spec_b[1],
                                           Sensitivity_oob  = Sens_Spec_oob[1],
                                           Specificity_b    = Sens_Spec_b[2],
                                           Specificity_oob  = Sens_Spec_oob[2],
                                           Kappa_b          = kap_b,
                                           Kappa_oob        = kap_oob,
                                           t(cm_b),
                                           t(cm_oob)
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

