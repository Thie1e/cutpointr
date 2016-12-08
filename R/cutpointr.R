#' Determine and evaluate optimal cutpoints
#'
#' @param x (numeric vector) The variable to be used for classification, e.g. test values.
#' @param class (vector) class is a binary vector of values indicating class membership.
#' @param group (vector) An additional covariate that identifies subgroups. Separate
#' optimal cutpoints will be determined by group.
#' @export
cutpointr <- function(...){
    UseMethod("cutpointr")
}

#' @importFrom purrr %>%
#' @export
cutpointr.default <- function(data, x, class, group, pos_class = NULL, higher = NULL,
                              optcut_func = optcut_emp_youden,
                              insert_midpoints = F, only_integer_cuts = F,
                              boot_runs = 0) {
    #
    # NSE
    #
    if (is.character(substitute(x))) x <- as.name(x)
    if (is.character(substitute(class))) class <- as.name(class)
    if (!missing(group) && is.character(substitute(group))) group <- as.name(group)
    x <- eval(substitute(x), data, parent.frame())
    class <- eval(substitute(class), data, parent.frame())
    if (!missing(group)) group <- eval(substitute(group), data, parent.frame())

    #
    # Prep
    #
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
    if (!is.factor(class)) class <- as.factor(class)
    if (length(unique(class)) != 2) stop(paste("Expecting two classes, got", length(unique(class))))
    if (is.null(pos_class)) {
        pos_class <- levels(class)[1]
        message(paste("Assuming", pos_class, "as positive class"))
    }
    neg_x <- x[class != pos_class]
    pos_x <- x[class == pos_class]
    if (is.null(higher)) {
        if (mean(neg_x) < mean(pos_x)) {
            message("Assuming the positive class has higher x values")
            higher <- TRUE
        } else {
            message("Assuming the positive class has lower x values")
            stop("higher = F not yet implemented")
            higher <- FALSE
        }
    }


    #
    # Calculate optimal cutpoint, map to cutpoint functions
    #
    ### Das hier könnte man evtl. in eine .default und eine .grouped_df Methode auslagern
    ### Mit mutate_ vor map_ bekäme man die gruppierten, nested Daten zum späteren
    ###     Plotten mit dazu
    if (!missing(group)) {
        g <- unique(group)
        ### Do we have to create this extra tibble?
        dat <- tibble::tibble(x, class, group)
        dat <- dat %>%
            dplyr::group_by_("group") %>%
            tidyr::nest_(data = ., key_col = "data", nest_cols = colnames(.)) %>%
            dplyr::mutate_(group = ~ as.character(group))
        optcut <- purrr::pmap_df(list(mod_names, optcut_func), function(n, f) {
            purrr::pmap_df(list(dat$group, dat$data), function(g, d) {
                optcut <- f(d$x, d$class, candidate_cuts = candidate_cuts,
                            higher = higher, pos_class = pos_class) %>%
                    dplyr::mutate_(method = ~ n,
                            group = ~ g)
            })
        })
    } else {
        dat <- tibble::tibble(x, class)
        dat <- dat %>%
            tidyr::nest_(data = ., key_col = "data", nest_cols = colnames(.))
        optcut <- purrr::pmap_df(list(mod_names, optcut_func), function(n, f) {
            purrr::map_df(dat$data, function(d) {
                optcut <- f(d$x, d$class, candidate_cuts = candidate_cuts,
                            higher = higher, pos_class = pos_class) %>%
                    dplyr::mutate_(method = ~ n)
            })
        })
    }

    #
    # Bootstrap to return distribution (all bootstrapped cuts?) of bootstrap runs
    # Also return LOO-Boot estimate
    #
    # optcut_func_partial <-purrr::partial(optcut_func, candidate_cuts = candidate_cuts,
    #                pos_class = pos_class, higher = higher)
    # replicate(10, function(x, class) {
    #     optcut_func_partial(sample(x, replace = T),
    #                         sample(class, replace = T))
    # })
    # if (boot_runs > 0) {
    #     # one bootstrap
    #     bi <- sample(1:length(x), size = x, replace = T)
    #     x <- x[bi]
    #     class <- class[bi]
    #     if (!missing(group)) {
    #         g <- unique(group)
    #         group <- group[bi]
    #         dat <- tibble(x, class, group)
    #         optcut <- dat %>%
    #             group_by_("group") %>%
    #             do_(~optcut_func(x = .$x, class = .$class,
    #                              candidate_cuts = candidate_cuts,
    #                              pos_class = pos_class, higher = higher))
    #     } else {
    #         optcut <- optcut_func(x = x, class = class,
    #                               candidate_cuts = candidate_cuts,
    #                               pos_class = pos_class, higher = higher)
    #     }
    # } else {
    #     boot_res <- NULL
    # }



    #
    # Bootstrap cutpoint variability and get LOO-Bootstrap performance estimate
    # Data are already nested and grouped if necessary
    #
    # bootstrap <- map2_df(mod_funcs, mod_names, function(f, n) {
    #     dat %>%
    #         transmute_(boot = ~ map(data, function(g) {
    #             map_df(1:10, function(rep) {
    #                 b_ind   <- sample(1:nrow(g), replace = T, size = nrow(g))
    #                 obs_b   <- g$status[b_ind]
    #                 x_b     <- g$elas[b_ind]
    #                 opt     <- f(x = x_b, status = obs_b)
    #                 optcut  <- extract_opt_cut(opt)
    #                 obs_oob <- g$status[-b_ind]
    #                 x_oob   <- g$elas[-b_ind]
    #                 # LOO-Bootstrap
    #                 Sensitivity_b = sens(obs = obs_b,  preds = x_b > optcut,
    #                                      pos_class = 1)
    #                 Specificity_b = spec(obs = obs_b,  preds = x_b > optcut,
    #                                      pos_class = 1)
    #                 # Youden_Index_b = Sensitivity_train + Specificity_train - 1
    #                 Sensitivity_oob = sens(obs = obs_oob,  preds = x_oob > optcut,
    #                                        pos_class = 1)
    #                 Specificity_oob = spec(obs = obs_oob,  preds = x_oob > optcut,
    #                                        pos_class = 1)
    #                 # Youden_Index_oob = Sensitivity_test + Specificity_test - 1
    #                 data.frame(opt,
    #                            Sensitivity_b, Specificity_b, #Youden_Index_train,
    #                            Sensitivity_oob, Specificity_oob #, Youden_Index_test
    #                 )
    #             })
    #         })) %>%
    #         mutate_(method = ~ n)
    # })



    #
    # Get n, prevalence, used data
    #



    res <- optcut

    # Join optcut and bootstrap by group and method
    # What happens if identical mehods were given? maybe not a good idea

    # res <- list(optimal_cutpoint = data.frame(optimal_cutpoint, metric with correct name),
    #             n,
    #             prev,
    #             used_data,
    #             possible bootstrap results,
    #             possible bootstrap validation results)
    return(res)
}