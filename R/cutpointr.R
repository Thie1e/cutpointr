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
#' @examples
#' library(OptimalCutpoints)
#' data(elas)
#' opt_cut <- cutpointr(elas, elas, status, gender, pos_class = 1, boot_runs = 500)
#' opt_cut
#' plot(opt_cut)
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
    if (!any(pos_class == class)) stop("Positive class not found in data")
    if (is.null(higher)) {
        neg_x <- x[class != pos_class]
        pos_x <- x[class == pos_class]
        if (mean(neg_x) < mean(pos_x)) {
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
            })
        })
        optcut <- tibble::as_tibble(optcut) # can pmap_df return a tibble so this is not necessary?
        optcut <- dplyr::full_join(optcut, dat, by = "group")
    } else {
        dat <- tibble::tibble(x, class)
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
                })
            })
        })
        optcut <- tibble::as_tibble(optcut)
        optcut <- dplyr::bind_cols(optcut, dat)
    }

    #
    # Bootstrap cutpoint variability and get LOO-Bootstrap performance estimate
    # Data are already nested and grouped if necessary
    #
    #### innermost map_df could be refactored into own function
    #
    if (boot_runs <= 0) {
        bootstrap <- NULL
    } else {
        bootstrap <- purrr::map2_df(optcut_func, mod_names, function(f, n) {
            dat %>%
                dplyr::transmute_(boot = ~ purrr::map(data, function(g) {
                    purrr::map_df(1:boot_runs, function(rep) {
                        b_ind   <- sample(1:nrow(g), replace = T, size = nrow(g))
                        obs_b   <- g$class[b_ind]
                        x_b     <- g$x[b_ind]
                        funcout_b <- f(x_b, obs_b, candidate_cuts = candidate_cuts,
                                    higher = higher, pos_class = pos_class)
                        optcut_b  <- extract_opt_cut(funcout_b)
                        obs_oob <- g$class[-b_ind]
                        x_oob   <- g$x[-b_ind]
                        # LOO-Bootstrap
                        preds_b <- ifelse(x_b > optcut_b, pos_class, "neg")
                        # Sensitivity_b = sens(obs = obs_b,  preds = preds_b,
                        #                      pos_class = pos_class)
                        # Specificity_b = spec(obs = obs_b,  preds = preds_b,
                        #                      pos_class = pos_class)
                        Sens_Spec_b = sens_spec(obs = obs_b,  preds = preds_b,
                                             pos_class = pos_class)
                        # Youden_Index_b = Sensitivity_train + Specificity_train - 1
                        preds_oob <- ifelse(x_oob > optcut_b, pos_class, "neg")
                        # Sensitivity_oob = sens(obs = obs_oob,  preds = preds_oob,
                        #                        pos_class = pos_class)
                        # Specificity_oob = spec(obs = obs_oob,  preds = preds_oob,
                        #                        pos_class = pos_class)
                        Sens_Spec_oob = sens_spec(obs = obs_oob,  preds = preds_oob,
                                               pos_class = pos_class)
                        # Youden_Index_oob = Sensitivity_test + Specificity_test - 1

                        # tibble::tibble(optcut_b,
                        #            # Sensitivity_b, Specificity_b, #Youden_Index_train,
                        #            # Sensitivity_oob, Specificity_oob #, Youden_Index_test
                        #            Sensitivity_b   = Sens_Spec_b[1],
                        #            Specificity_b   = Sens_Spec_b[2],
                        #            Sensitivity_oob = Sens_Spec_oob[1],
                        #            Specificity_oob = Sens_Spec_oob[2]
                        # )

                        tibble::as_data_frame(cbind(funcout_b,
                                   Sensitivity_b   = Sens_Spec_b[1],
                                   Specificity_b   = Sens_Spec_b[2],
                                   Sensitivity_oob = Sens_Spec_oob[1],
                                   Specificity_oob = Sens_Spec_oob[2]
                        ))
                    })
                })) # %>%
                # dplyr::mutate_(method = ~ n) # As a check, is already in optcut
        })
    }

    res <- dplyr::bind_cols(optcut, bootstrap)
    class(res) <- c("cutpointr", class(res))

    return(res)
}


