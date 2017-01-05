#' Determine and evaluate optimal cutpoints
#' @param x (numeric vector) The variable to be used for classification, e.g. test values.
#' @param class (vector) class is a binary vector of values indicating class membership.
#' @param subgroup (vector) An additional covariate that identifies subgroups. Separate
#' optimal cutpoints will be determined by group.
#' @export
cutpointr <- function(...){
    UseMethod("cutpointr")
}

#' Determine and evaluate optimal cutpoints
#' @importFrom purrr %>%
#' @importFrom doRNG %dorng%
#' @importFrom foreach %do%
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
#' ## Optimal cutpoint for elas, as before, but for the separate subgroups
#' opt_cut <- cutpointr(elas, elas, status, gender)
#' opt_cut
#' plot(opt_cut)
#'
#' ## Bootstrapping to assess cutpoint variability and out-of-sample performance
#' opt_cut <- cutpointr(elas, elas, status, boot_runs = 200)
#' opt_cut
#' plot(opt_cut)
#'
#' ## Bootstrapping also works on individual subgroups
#' opt_cut <- cutpointr(elas, elas, status, gender, boot_runs = 200)
#' opt_cut
#' plot(opt_cut)
#'
#' ## Different cutpoint function / metric
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
#' opt_cut_na <- cutpointr(elas_na, elas, status, gender,
#'                   boot_runs = 200, na.rm = T)
#' opt_cut_na
#'
#' ## Parallelized bootstrapping
#' library(doSNOW)
#' cl <- makeCluster(parallel::detectCores())
#' registerDoSNOW(cl)
#' opt_cut <- cutpointr(elas, elas, status, gender, pos_class = 1,
#'                boot_runs = 2000, allowParallel = TRUE)
#' opt_cut
#' plot(opt_cut)
#'
#' ## Cutoff for model prediction
#' library(caret)
#' library(randomForest)
#' mod <- train(y = spam$type, x = spam[, 1:57], method = "rpart", preProcess = "nzv",
#'            trControl = trainControl(savePredictions = TRUE, classProbs = TRUE, number = 5))
#' mod_cut <- cutpointr(mod$pred, spam, obs, boot_runs = 200)
#'
#' ## Wrapper for optimal.cutpoints
#' opt_cut <- cutpointr(elas, elas, status, gender, pos_class = 1, boot_runs = 2000,
#'                      optcut_func = oc_OptimalCutpoints, methods = "Youden", allowParallel = T)
#' # OptimalCutpoints finds different cutpoints because candidate_cuts per subgroup
#' opt_cut
#' plot(opt_cut)
#'
#' @export
cutpointr.default <- function(data, x, class, subgroup, pos_class = NULL,
                              neg_class = NULL, direction = NULL,
                              optcut_func = oc_youden,
                              insert_midpoints = FALSE,
                              candidate_cuts = NULL,
                              boot_runs = 0, na.rm = FALSE, allowParallel = FALSE, ...) {
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
        # if (is.null(mod_names)) {
            mod_names <- as.character(substitute(optcut_func))
            mod_names <- mod_names[1]
        # } else {
        #     if (is.symbol(mod_names)) {
        #         # a single function was given:
        #         mod_names <- as.character(substitute(mod_names))
        #     } else if (mod_names[[1]] == "list") {
        #         # if a list of functions is given:
        #         mod_names <- lapply(seq_along(mod_names)[-1], function(i) mod_names[[i]])
        #         mod_names <- as.character(mod_names)
        #     } else if (is.null(mod_names)) {
        #         stop("Could not get the names of the cutpoint function(s)")
        #     }
        # }
    }
    if (is.null(mod_names)) stop("Could not get the names of the cutpoint function(s)")
    # if (!is.list(optcut_func)) optcut_func <- list(optcut_func)

    #
    # Prep
    #

    #NA
    if (any(anyNA(c(x, class)) | (!missing(subgroup) && anyNA(subgroup))) &&
         (missing(na.rm) | !na.rm)) {
        warning("NAs found but na.rm = FALSE")
    }

    # Check classes
    # if(na.rm) uc <- unique(na.omit(class)) else uc <- unique(class)
    # luc <- length(uc)
    # if (luc != 2) stop(paste("Expecting two classes, got", luc))

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
    if (na.rm) candidate_cuts <- na.omit(candidate_cuts)
    candidate_cuts <- inf_to_candidate_cuts(candidate_cuts, direction)

    #
    # Calculate optimal cutpoint, map to cutpoint functions
    #
    if (!missing(subgroup)) {
        dat <- tibble::tibble(x, class, subgroup)
        if (na.rm) dat <- na.omit(dat)
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
        # optcut <- purrr::pmap_df(list(mod_names, optcut_func), function(n, f) {
        optcut <- purrr::pmap_df(list(dat$subgroup, dat$data), function(g, d) {
            optcut <- optcut_func(d, x = "x", class = "class", candidate_cuts = candidate_cuts,
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
        if (na.rm) dat <- na.omit(dat)
        dat <- dat %>%
            tidyr::nest_(data = ., key_col = "data", nest_cols = colnames(.)) %>%
            dplyr::mutate_(pos_class = ~ pos_class,
                           prevalence = ~ purrr::map_dbl(data, function(g) {
                               mean(g$class == pos_class)
                               })
                           )
        optcut <- purrr::map_df(dat$data, function(d) {
            optcut <- optcut_func(d, "x", "class", candidate_cuts = candidate_cuts,
                        direction = direction, pos_class = pos_class, ...)
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
    #### innermost map_df could be refactored into own function
    #
    `%seq_or_par%` <- ifelse(allowParallel, `%dorng%`, `%do%`)
    if (boot_runs <= 0) {
        bootstrap <- NULL
    } else {
        bootstrap <- dat %>%
            dplyr::transmute_(boot = ~ purrr::map(data, function(g) {
                boot_g <- foreach::foreach(rep = 1:boot_runs, .combine = rbind,
                    .packages = "OptimalCutpoints",
                    .export = c("f", "n", "direction", "pos_class",
                    "neg_class", "candidate_cuts", "metric_names")) %seq_or_par%
                    {
                        b_ind   <- sample(1:nrow(g), replace = T, size = nrow(g))
                        if (length(unique(g[b_ind, ]$class)) == 1) {
                            funcout_b <- data.frame(NA, NA)
                            colnames(funcout_b) <- c("optimal_cutpoint", mn)
                            optcut_b <- NA
                        } else {
                            funcout_b <- optcut_func(g[b_ind, ], "x", "class",
                                                     candidate_cuts = candidate_cuts,
                                                     direction = direction,
                                                     pos_class = pos_class, ...)
                            optcut_b  <- extract_opt_cut(funcout_b)
                        }
                        # LOO-Bootstrap
                        preds_b <- ifelse(g$x[b_ind] > optcut_b, pos_class, neg_class)
                        Sens_Spec_b = sens_spec(obs = g$class[b_ind],  preds = preds_b,
                                                pos_class = pos_class)
                        preds_oob <- ifelse(g$x[-b_ind] > optcut_b, pos_class, neg_class)
                        Sens_Spec_oob = sens_spec(obs = g$class[-b_ind],  preds = preds_oob,
                                                  pos_class = pos_class)
                        bootstrap <- tibble::as_data_frame(cbind(funcout_b,
                                                                 Sensitivity_b   = Sens_Spec_b[1],
                                                                 Specificity_b   = Sens_Spec_b[2],
                                                                 Sensitivity_oob = Sens_Spec_oob[1],
                                                                 Specificity_oob = Sens_Spec_oob[2]
                        ))
                        return(bootstrap)
                    }
                if (anyNA(boot_g)) {
                    warning("Missing values in bootstrap, probably due to sampling of only one class")
                }
                return(boot_g)
            }))
    }

    res <- dplyr::bind_cols(optcut, bootstrap)
    class(res) <- c("cutpointr", class(res))

    return(res)
}


