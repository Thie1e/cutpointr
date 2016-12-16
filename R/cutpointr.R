#' Determine and evaluate optimal cutpoints
#' @param x (numeric vector) The variable to be used for classification, e.g. test values.
#' @param class (vector) class is a binary vector of values indicating class membership.
#' @param group (vector) An additional covariate that identifies subgroups. Separate
#' optimal cutpoints will be determined by group.
#' @export
cutpointr <- function(...){
    UseMethod("cutpointr")
}

#' Determine and evaluate optimal cutpoints
#' @importFrom purrr %>%
#' @importFrom doRNG %dorng%
#' @importFrom foreach %do%
#' @export
#' @examples
#' library(OptimalCutpoints)
#' data(elas)
#' opt_cut <- cutpointr(elas, elas, status, gender, pos_class = 1, boot_runs = 500)
#' opt_cut
#' plot(opt_cut)
#'
#' ## With NAs
#' elas_na <- elas
#' elas_na$elas[10] <- NA
#' elas_na$status[20] <- NA
#' elas_na$gender[30] <- NA
#' opt_cut_na <- cutpointr(elas_na, elas, status, gender, pos_class = 1,
#'                   boot_runs = 500, na.rm = T)
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
cutpointr.default <- function(data, x, class, group, pos_class = NULL,
                              neg_class = NULL, higher = NULL,
                              optcut_func = optcut_emp_youden,
                              insert_midpoints = FALSE, only_integer_cuts = FALSE,
                              boot_runs = 0, na.rm = FALSE, allowParallel = FALSE) {
    #
    # NSE
    #
    if (is.character(substitute(x))) x <- as.name(x)
    if (is.character(substitute(class))) class <- as.name(class)
    if (!missing(group) && is.character(substitute(group))) group <- as.name(group)
    predictor <- deparse(substitute(x))
    outcome   <- deparse(substitute(class))
    x <- eval(substitute(x), data, parent.frame())
    class <- eval(substitute(class), data, parent.frame())
    if (!missing(group)) {
        group_var <- deparse(substitute(group))
        group <- eval(substitute(group), data, parent.frame())
    }

    #
    # Prep
    #
    if (any(anyNA(c(x, class)) | (!missing(group) && anyNA(group))) &&
         (missing(na.rm) | !na.rm)) {
        warning("NAs found but na.rm = FALSE")
    }
    if (length(optcut_func) > 1 && length(unique(sapply(optcut_func, class))) != 1) {
        stop("optcut_func should be a character vector, a list of functions, or a function. Do not mix types.")
    }
    if (is.character(optcut_func)) {
        # If a character vec is given the user surely wants to search in the package
        mod_names <- optcut_func
        optcut_func <- paste0("cutpointr::", optcut_func)
        optcut_func <- lapply(optcut_func, function(fun) eval(parse(text = fun)))
    } else {
        cl <- match.call()
        mod_names <- cl$optcut_func
        # if default was not changed:
        if (is.null(mod_names)) {
            mod_names <- as.character(substitute(optcut_func))
        } else {
            if (is.symbol(mod_names)) {
                # a single function was given:
                mod_names <- as.character(substitute(mod_names))
            } else if (mod_names[[1]] == "list") {
                # if a list of functions is given:
                mod_names <- lapply(seq_along(mod_names)[-1], function(i) mod_names[[i]])
                mod_names <- as.character(mod_names)
            } else if (is.null(mod_names)) {
                stop("Could not get the names of the cutpoint function(s)")
            }
        }
    }
    if (is.null(mod_names)) stop("Could not get the names of the cutpoint function(s)")
    if (!is.list(optcut_func)) optcut_func <- list(optcut_func)
    if (insert_midpoints) {
        candidate_cuts <- unique(insert_midpoints(x))
    } else {
        candidate_cuts <- unique(x)
    }
    if (only_integer_cuts) stop("Not yet implemented")
    # if (!is.factor(class)) class <- as.factor(class)
    luc <- ifelse(na.rm, length(unique(na.omit(class))), length(unique(class)))
    if (luc != 2) stop(paste("Expecting two classes, got", luc))
    if (is.null(pos_class)) {
        pos_class <- class[1]
        message(paste("Assuming", pos_class, "as positive class"))
    }
    # pos_class <- as.character(pos_class)
    if (!any(pos_class == class)) stop("Positive class not found in data")
    # neg_class <- levels(class)[levels(class) != pos_class]
    if (is.null(neg_class)) {
        neg_class <- unique(class)
        neg_class <- neg_class[neg_class != pos_class]
    }
    if (is.null(higher)) {
        if (mean(na.omit(x[class != pos_class])) < mean(na.omit(x[class == pos_class]))) {
            message("Assuming the positive class has higher x values")
            higher <- TRUE
        } else {
            message("Assuming the positive class has lower x values")
            higher <- FALSE
        }
    }

    #
    # Calculate optimal cutpoint, map to cutpoint functions
    #
    ### Das hier könnte man evtl. in eine .default und eine .grouped_df Methode auslagern
    ### (auch die anonymen Funktionen in map)
    if (!missing(group)) {
        g <- unique(group)
        ### Do we have to create this extra tibble?
        dat <- tibble::tibble(x, class, group)
        if (na.rm) dat <- na.omit(dat)
        dat <- dat %>%
            dplyr::group_by_("group") %>%
            # dplyr::mutate_(prevalence = ~ mean(class == pos_class)) %>%
            tidyr::nest_(data = ., key_col = "data", nest_cols = colnames(.)) %>%
            dplyr::mutate_(group = ~ as.character(group),
                           pos_class = ~ pos_class,
                           prevalence = ~ purrr::map_dbl(data, function(g) {
                               mean(g$class == pos_class)
                               })
                           )
        optcut <- purrr::pmap_df(list(mod_names, optcut_func), function(n, f) {
            purrr::pmap_df(list(dat$group, dat$data), function(g, d) {
                optcut <- f(d$x, d$class, candidate_cuts = candidate_cuts,
                            higher = higher, pos_class = pos_class) %>%
                    dplyr::mutate_(method = ~ n,
                                   group = ~ g)
                                   # direction = ~ ifelse(higher, ">", "<"))
            })
        })
        optcut <- tibble::as_tibble(optcut) # can pmap_df return a tibble so this is not necessary?
        optcut <- dplyr::full_join(optcut, dat, by = "group")
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
        optcut <- purrr::pmap_df(list(mod_names, optcut_func), function(n, f) {
            purrr::pmap_df(list(mod_names, optcut_func), function(n, f) {
                purrr::map_df(dat$data, function(d) {
                    optcut <- f(d$x, d$class, candidate_cuts = candidate_cuts,
                                higher = higher, pos_class = pos_class) %>%
                        dplyr::mutate_(method = ~ n)
                                       # direction = ~ ifelse(higher, ">", "<"))
                })
            })
        })
        optcut <- tibble::as_tibble(optcut)
        optcut <- dplyr::bind_cols(optcut, dat)
    }

    optcut$direction                     <- ifelse(higher, ">", "<")
    optcut$predictor                     <- predictor
    optcut$outcome                       <- outcome
    optcut$neg_class                     <- neg_class
    if (!missing(group)) optcut$grouping <- group_var

    # Reorder for nicer output
    mn <- find_metric_name(colnames(optcut))
    select_cols <- c("group", "direction", "optimal_cutpoint", mn, "method",
                         "pos_class", "neg_class", "prevalence",
                         "outcome", "predictor", "grouping", "data")
    # group and grouping may not be given
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
        bootstrap <- purrr::map2_df(optcut_func, mod_names, function(f, n) {
            dat %>%
                dplyr::transmute_(boot = ~ purrr::map(data, function(g) {
                    # purrr::map_df(1:boot_runs, function(rep) {
                    foreach::foreach(rep = 1:boot_runs, .combine = rbind,
                            .export = c("f", "n", "higher", "pos_class",
                                        "neg_class", "candidate_cuts")) %seq_or_par% {
                        b_ind   <- sample(1:nrow(g), replace = T, size = nrow(g))
                        obs_b   <- g$class[b_ind]
                        x_b     <- g$x[b_ind]
                        funcout_b <- f(x_b, obs_b, candidate_cuts = candidate_cuts,
                                       higher = higher, pos_class = pos_class)
                        optcut_b  <- extract_opt_cut(funcout_b)
                        obs_oob <- g$class[-b_ind]
                        x_oob   <- g$x[-b_ind]
                        # LOO-Bootstrap
                        preds_b <- ifelse(x_b > optcut_b, pos_class, neg_class)
                        Sens_Spec_b = sens_spec(obs = obs_b,  preds = preds_b,
                                                pos_class = pos_class)
                        preds_oob <- ifelse(x_oob > optcut_b, pos_class, neg_class)
                        Sens_Spec_oob = sens_spec(obs = obs_oob,  preds = preds_oob,
                                                  pos_class = pos_class)
                        tibble::as_data_frame(cbind(funcout_b,
                                                    Sensitivity_b   = Sens_Spec_b[1],
                                                    Specificity_b   = Sens_Spec_b[2],
                                                    Sensitivity_oob = Sens_Spec_oob[1],
                                                    Specificity_oob = Sens_Spec_oob[2]
                        ))
                    }
                })) # %>%
            # dplyr::mutate_(method = ~ n) # As a check, is already in optcut
        })
    }

    res <- dplyr::bind_cols(optcut, bootstrap)
    class(res) <- c("cutpointr", class(res))

    return(res)
}


