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
#'  \item candidate_cuts
#'  \item pos_class
#'  \item neg_class
#'  \item direction
#' }
#'
#' The ... argument can be used to avoid an error if not all of the above
#' arguments are needed.
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
#' opt_cut <- cutpointr(elas, elas, status, direction = "<", pos_class = 0)
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
#' ## Different cutpoint function / metric
#' set.seed(123)
#' opt_cut <- cutpointr(elas, elas, status, gender, pos_class = 1, boot_runs = 200,
#'                      optcut_func = oc_equalsesp)
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
#' cl <- makeCluster(parallel::detectCores())
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
#'                      optcut_func = oc_OptimalCutpoints, methods = "Youden",
#'                      allowParallel = TRUE)
#' # OptimalCutpoints finds different cutpoints because candidate_cuts per subgroup
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
#' @param candidate_cuts (numeric vector) By default the unique values in x will
#' be used as potential cutoffs. Alternatively, a vector of cutoffs to be used can
#' be supplied using the candidate_cuts argument.
#' @param boot_runs (numeric, optional) If positive, this number of bootstrap samples
#' will be used to assess the variability and the out-of-sample performance.
#' @param na.rm (logical) Set to TRUE to keep only complete cases of x, class and
#' subgroup (if specified). Missing values with na.rm = FALSE (default) will
#' raise an error.
#' @param allowParallel (logical) If TRUE, the bootstrapping will be parallelized
#' using foreach. A local cluster, for example, should have been started manually
#' beforehand.
#' @param ... Further optional arguments that will be passed to optcut_func.
#' @importFrom purrr %>%
#' @importFrom foreach %do%
#' @export
cutpointr <- function(data, x, class, subgroup, pos_class = NULL,
                              neg_class = NULL, direction = NULL,
                              optcut_func = oc_youden, candidate_cuts = NULL,
                              boot_runs = 0, na.rm = FALSE,
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
    if (length(optcut_func) > 1 | !(class(optcut_func) %in% c("character", "function"))) {
        stop("optcut_func should be character string or a function")
    }
    if (is.character(optcut_func)) {
        # If a character vec is given the user surely wants to search in the package
        mod_names <- optcut_func[1]
        optcut_func <- paste0("cutpointr::", optcut_func)
        optcut_func <- lapply(optcut_func, function(fun) eval(parse(text = fun)))
    } else {
        cl <- match.call()
        mod_names <- cl$optcut_func
        # if default was not changed:
            mod_names <- as.character(substitute(optcut_func))
            mod_names <- mod_names[1]
    }
    if (is.null(mod_names)) stop("Could not get the names of the cutpoint function(s)")

    #
    # Prep
    #

    #NA
    if (any(anyNA(c(x, class)) | (!missing(subgroup) && anyNA(subgroup))) &&
         (missing(na.rm) | !na.rm)) {
        warning("NAs found but na.rm = FALSE")
    }

    # Determine direction and/or pos_class if necessary:
    assumptions <- assume_direction_pos_class(x = x, class = class,
                                              pos_class = pos_class,
                                              neg_class = neg_class,
                                              direction = direction,
                                              na.rm = na.rm)
    if (is.null(direction)) direction <- assumptions$direction
    if (is.null(pos_class)) pos_class <- assumptions$pos_class
    if (is.null(neg_class)) neg_class <- assumptions$neg_class

    # Save candidate cuts
    if (is.null(candidate_cuts)) candidate_cuts <- unique(x)
    if (na.rm) candidate_cuts <- stats::na.omit(candidate_cuts)
    candidate_cuts <- inf_to_candidate_cuts(candidate_cuts, direction)

    #
    # Calculate optimal cutpoint, map to cutpoint functions
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
            optcut <- optcut_func(data = d, x = "x", class = "class",
                                  candidate_cuts = candidate_cuts,
                                  direction = direction, pos_class = pos_class,
                                  neg_class = neg_class, ...) %>%
                dplyr::mutate_(subgroup = ~ g)
            sesp <- sesp_from_oc(x = d$x, class = d$class,
                                 oc = optcut$optimal_cutpoint,
                                 direction = direction, pos_class = pos_class,
                                 neg_class = neg_class)
            optcut$sensitivity <- sesp["Sensitivity"]
            optcut$specificity <- sesp["Specificity"]
            return(optcut)
        })
        # })
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
            optcut <- optcut_func(data = d,  x = "x", class = "class",
                                  candidate_cuts = candidate_cuts,
                                  direction = direction, pos_class = pos_class,
                                  neg_class = neg_class, ...)
            sesp <- sesp_from_oc(x = d$x, class = d$class,
                                 oc = optcut$optimal_cutpoint,
                                 direction = direction, pos_class = pos_class,
                                 neg_class = neg_class)
            optcut$sensitivity = sesp["Sensitivity"]
            optcut$specificity = sesp["Specificity"]
            return(optcut)
        })
        optcut <- tibble::as_tibble(optcut)
        optcut <- dplyr::bind_cols(optcut, dat)
    }

    optcut$direction                     <- direction
    optcut$predictor                     <- predictor
    optcut$outcome                       <- outcome
    optcut$neg_class                     <- neg_class
    optcut$method                        <- mod_names
    if (!missing(subgroup)) optcut$grouping <- subgroup_var

    # Reorder for nicer output
    mn <- find_metric_name(colnames(optcut))
    select_cols <- c("subgroup", "direction", "optimal_cutpoint",
                     mn, "sensitivity", "specificity", "method",
                     "pos_class", "neg_class", "prevalence",
                     "outcome", "predictor", "grouping", "data")
    # subgroup and grouping may not be given
    select_cols <- select_cols[select_cols %in% colnames(optcut)]
    optcut <- optcut[, select_cols]

    #
    # Bootstrap cutpoint variability and get LOO-Bootstrap performance estimate
    # Data are already nested and grouped if necessary
    #
    if (allowParallel) {
        `%seq_or_par%` <- doRNG::`%dorng%`
    } else {
        `%seq_or_par%` <- `%do%`
    }
    if (boot_runs <= 0) {
        bootstrap <- NULL
    } else {
        bootstrap <- dat %>%
            dplyr::transmute_(boot = ~ purrr::map(data, function(g) {
                boot_g <- foreach::foreach(rep = 1:boot_runs, .combine = rbind,
                    .packages = "OptimalCutpoints",
                    .export = c("optcut_func", "direction", "pos_class",
                    "neg_class", "candidate_cuts", "mn")) %seq_or_par%
                    {
                        b_ind   <- sample(1:nrow(g), replace = T, size = nrow(g))
                        if (length(unique(g[b_ind, ]$class)) == 1) {
                            funcout_b <- data.frame(NA, NA)
                            colnames(funcout_b) <- c("optimal_cutpoint", mn)
                            optcut_b <- NA
                        } else {
                            funcout_b <- optcut_func(data = g[b_ind, ], x = "x",
                                                     class = "class",
                                                     candidate_cuts = candidate_cuts,
                                                     direction = direction,
                                                     pos_class = pos_class,
                                                     neg_class = neg_class, ...)
                            optcut_b  <- extract_opt_cut(funcout_b)
                        }
                        # LOO-Bootstrap
                        # preds_b <- ifelse(g$x[b_ind] > optcut_b, pos_class, neg_class)
                        preds_b <- ifel_pos_neg(g$x[b_ind] > optcut_b, pos_class, neg_class)
                        cm_b <- conf_mat(obs = g$class[b_ind],  preds = preds_b,
                                                pos_class = pos_class)
                        Sens_Spec_b <- sens_spec(tp = cm_b["TP"], fp = cm_b["FP"],
                                                tn = cm_b["TN"], fn = cm_b["FN"])
                        Acc_b <- accuracy(tp = cm_b["TP"], fp = cm_b["FP"],
                                          tn = cm_b["TN"], fn = cm_b["FN"])
                        # preds_oob <- ifelse(g$x[-b_ind] > optcut_b, pos_class, neg_class)
                        preds_oob <- ifel_pos_neg(g$x[-b_ind] > optcut_b, pos_class, neg_class)
                        cm_oob <- conf_mat(obs = g$class[-b_ind],  preds = preds_oob,
                                           pos_class = pos_class)
                        Sens_Spec_oob <- sens_spec(tp = cm_oob["TP"], fp = cm_oob["FP"],
                                                   tn = cm_oob["TN"], fn = cm_oob["FN"])
                        Acc_oob <- accuracy(tp = cm_oob["TP"], fp = cm_oob["FP"],
                                            tn = cm_oob["TN"], fn = cm_oob["FN"])
                        bootstrap <- tibble::as_data_frame(cbind(funcout_b,
                                                                 Sensitivity_b   = Sens_Spec_b[1],
                                                                 Specificity_b   = Sens_Spec_b[2],
                                                                 Accuracy_b      = Acc_b,
                                                                 Sensitivity_oob = Sens_Spec_oob[1],
                                                                 Specificity_oob = Sens_Spec_oob[2],
                                                                 Accuracy_oob    = Acc_oob
                        ))
                        return(bootstrap)
                    }
                lna <- sum(is.na(boot_g))
                if (lna) warning(paste(lna, "missing values in bootstrap, probably due to sampling of only one class"))
                return(boot_g)
            }))
    }

    res <- dplyr::bind_cols(optcut, bootstrap)
    class(res) <- c("cutpointr", class(res))

    return(res)
}


