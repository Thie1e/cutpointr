test_that("Cutpointr returns a cutpointr without NAs and a certain Nr of rows", {
    data(suicide)
    opt_cut <- cutpointr(suicide, dsi, suicide)
    expect_true("cutpointr" %in% class(opt_cut))
    expect_that(nrow(opt_cut), equals(1))
    expect_that(sum(is.na(opt_cut)), equals(0))
    expect_silent(plot(opt_cut))
    expect_silent(plot_metric(opt_cut))
    expect_silent(plot_roc(opt_cut))
    expect_silent(plot_x(opt_cut))
    expect_silent(plot_precision_recall(opt_cut))
})

test_that("Cutpointr works with different data types", {
    set.seed(456)
    tempdat <- data.frame(x = rnorm(10),
                          y = sample(0:1, size = 10, replace = TRUE))
    opt_cut <- cutpointr(tempdat, x, y)
    expect_that(nrow(opt_cut), equals(1))
    expect_that(sum(is.na(opt_cut)), equals(0))
    expect_silent(plot(opt_cut))

    tempdat$y <- factor(tempdat$y)
    opt_cut <- cutpointr(tempdat, x, y)
    expect_that(nrow(opt_cut), equals(1))
    expect_that(sum(is.na(opt_cut)), equals(0))
    expect_silent(plot(opt_cut))

    tempdat$y <- as.character(tempdat$y)
    opt_cut <- cutpointr(tempdat, x, y)
    expect_that(nrow(opt_cut), equals(1))
    expect_that(sum(is.na(opt_cut)), equals(0))
    expect_silent(plot(opt_cut))

    # With subgroup
    set.seed(567)
    tempdat <- data.frame(x = rnorm(30),
                          y = sample(0:1, size = 30, replace = TRUE),
                          g = sample(0:2, size = 30, replace = TRUE))
    opt_cut <- cutpointr(tempdat, x, y, g)
    expect_that(nrow(opt_cut), equals(3))
    expect_that(sum(is.na(opt_cut)), equals(0))
    expect_silent(plot(opt_cut))
    expect_silent(plot_metric(opt_cut))
    expect_silent(plot_roc(opt_cut))
    expect_silent(plot_x(opt_cut))
    expect_silent(plot_precision_recall(opt_cut))

    tempdat$g <- factor(tempdat$g)
    opt_cut <- cutpointr(tempdat, x, y, g)
    expect_that(nrow(opt_cut), equals(3))
    expect_that(sum(is.na(opt_cut)), equals(0))
    expect_silent(plot(opt_cut))
    expect_silent(plot_metric(opt_cut))
    expect_silent(plot_roc(opt_cut))
    expect_silent(plot_x(opt_cut))
    expect_silent(plot_precision_recall(opt_cut))

    tempdat$g <- as.character(tempdat$g)
    opt_cut <- cutpointr(tempdat, x, y, g)
    expect_that(nrow(opt_cut), equals(3))
    expect_that(sum(is.na(opt_cut)), equals(0))
    expect_silent(plot(opt_cut))
    expect_silent(plot_metric(opt_cut))
    expect_silent(plot_roc(opt_cut))
    expect_silent(plot_x(opt_cut))
    expect_silent(plot_precision_recall(opt_cut))
})

test_that("Bootstrap does not return duplicate colnames", {
    set.seed(456)
    tempdat <- data.frame(x = rnorm(100),
                          y = sample(0:1, size = 100, replace = TRUE))
    opt_cut <- cutpointr(tempdat, x, y, boot_runs = 20)
    expect_true(all(table(colnames(opt_cut$boot[[1]])) == 1))

    # With subgroup
    set.seed(123)
    tempdat <- data.frame(x = rnorm(300),
                          y = sample(0:1, size = 300, replace = TRUE),
                          g = sample(0:2, size = 300, replace = TRUE))
    opt_cut <- cutpointr(tempdat, x, y, g, boot_runs = 20)
    expect_true(all(table(colnames(opt_cut$boot[[1]])) == 1))
})

test_that("Plotting with bootstrapping is silent", {
    set.seed(456)
    tempdat <- data.frame(x = rnorm(100),
                          y = sample(0:1, size = 100, replace = TRUE))
    opt_cut <- cutpointr(tempdat, x, y, boot_runs = 20)
    expect_silent(plot(opt_cut))
    expect_silent(plot_metric(opt_cut))
    expect_silent(plot_roc(opt_cut))
    expect_silent(plot_cut_boot(opt_cut))
    expect_silent(plot_metric_boot(opt_cut))
    expect_silent(plot_x(opt_cut))
    expect_silent(plot_precision_recall(opt_cut))

    # With subgroup
    set.seed(123)
    tempdat <- data.frame(x = rnorm(300),
                          y = sample(0:1, size = 300, replace = TRUE),
                          g = sample(0:2, size = 300, replace = TRUE))
    opt_cut <- cutpointr(tempdat, x, y, g, boot_runs = 20)
    expect_silent(plot(opt_cut))
    expect_silent(plot_metric(opt_cut))
    expect_silent(plot_roc(opt_cut))
    expect_silent(plot_cut_boot(opt_cut))
    expect_silent(plot_metric_boot(opt_cut))
    expect_silent(plot_x(opt_cut))
    expect_silent(plot_precision_recall(opt_cut))
})

test_that("AUC calculation is correct and works with Inf and -Inf", {
    tempdat <- data.frame(x = c(-Inf, 0.3, Inf),
                          y = factor(c(0, 1, 1)))
    roc_cutpointr <- cutpointr::roc(tempdat, "x", "y", pos_class = 1, neg_class = 0)
    auc_cutpointr <- cutpointr:::auc(roc_cutpointr$tpr, roc_cutpointr$fpr)
    expect_equal(auc_cutpointr, 1)

    set.seed(123)
    tempdat <- data.frame(x = runif(100),
                          y = factor(sample(0:1, size = 100, replace = TRUE)))
    roc_cutpointr <- cutpointr::roc(tempdat, "x", "y", pos_class = 1, neg_class = 0)
    auc_cutpointr <- cutpointr:::auc(roc_cutpointr$tpr, roc_cutpointr$fpr)
    expect_equal(auc_cutpointr, 0.428)
})

test_that("Correct midpoints are found", {
    temp <- data.frame(x = c(-Inf, 1, 2, 3, 5, Inf), y = c(1, 1, 1, 0, 0, 0))
    optcut <- cutpointr(temp, x, y, use_midpoints = TRUE, pos_class = 1)
    expect_equal(optcut$optimal_cutpoint, 2.5)
    expect_warning(plot(optcut))
    optcut <- cutpointr(temp, x, y, use_midpoints = TRUE, pos_class = 0)
    expect_equal(optcut$optimal_cutpoint, 2.5)
})

test_that("find_metric_name finds metric", {
    set.seed(123)
    tempdat <- data.frame(x = runif(100),
                          y = factor(sample(0:1, size = 100, replace = TRUE)))
    optcut <- cutpointr(tempdat, x, y, method = maximize_metric, metric = youden)
    expect_equal(cutpointr:::find_metric_name(optcut), "Youden_Index")
    set.seed(1234)
    tempdat <- data.frame(x = runif(100),
                          y = factor(sample(0:1, size = 100, replace = TRUE)),
                          g = factor(sample(0:1, size = 100, replace = TRUE)))
    optcut <- cutpointr(tempdat, x, y, g, method = maximize_metric, metric = youden)
    expect_equal(cutpointr:::find_metric_name(optcut), "Youden_Index")
    if (require(OptimalCutpoints)) {
        optcut <- cutpointr(tempdat, x, y, g, method = oc_OptimalCutpoints,
                            oc_metric = "Youden")
        expect_equal(cutpointr:::find_metric_name(optcut), "Youden")
    }
})

test_that("no duplicate column names are returned", {
    set.seed(123)
    tempdat <- data.frame(x = runif(100),
                          y = factor(sample(0:1, size = 100, replace = TRUE)))
    optcut <- cutpointr(tempdat, x, y, method = oc_youden_normal)
    expect_true(all(table(colnames(optcut)) == 1))
    expect_silent(plot(optcut))
    if (require(fANCOVA)) {
        optcut <- cutpointr(tempdat, x, y, method = oc_youden_kernel)
        expect_true(all(table(colnames(optcut)) == 1))
        expect_silent(plot(optcut))
    }
    optcut <- cutpointr(tempdat, x, y)
    expect_true(all(table(colnames(optcut)) == 1))
    expect_silent(plot(optcut))
    optcut <- cutpointr(tempdat, x, y, method = oc_manual, cutpoint = 30)
    expect_true(all(table(colnames(optcut)) == 1))
    expect_silent(plot(optcut))
    if (require(OptimalCutpoints)) {
        optcut <- cutpointr(tempdat, x, y, method = oc_OptimalCutpoints,
                            oc_metric = "Youden")
        expect_true(all(table(colnames(optcut)) == 1))
        expect_silent(plot(optcut))
    }
    set.seed(1234)
    tempdat <- data.frame(x = runif(100),
                          y = factor(sample(0:1, size = 100, replace = TRUE)),
                          g = factor(sample(0:1, size = 100, replace = TRUE)))
    optcut <- cutpointr(tempdat, x, y, g, method = oc_youden_normal)
    expect_true(all(table(colnames(optcut)) == 1))
    expect_silent(plot(optcut))
    if (require(fANCOVA)) {
        optcut <- cutpointr(tempdat, x, y, g, method = oc_youden_kernel)
        expect_true(all(table(colnames(optcut)) == 1))
        expect_silent(plot(optcut))
    }
    optcut <- cutpointr(tempdat, x, y, g)
    expect_true(all(table(colnames(optcut)) == 1))
    expect_silent(plot(optcut))
    optcut <- cutpointr(tempdat, x, y, g, method = oc_manual, cutpoint = 30)
    expect_true(all(table(colnames(optcut)) == 1))
    expect_silent(plot(optcut))
    if (require(OptimalCutpoints)) {
        optcut <- cutpointr(tempdat, x, y, g, method = oc_OptimalCutpoints,
                            oc_metric = "Youden")
        expect_true(all(table(colnames(optcut)) == 1))
        expect_silent(plot(optcut))
    }
})

test_that("Correct cutpoints with example data", {
    exdat <- data.frame(obs = c(0, 0, 1, 1),
                        preds = c(0, 0, 1, 1))
    optcut <- cutpointr(exdat, preds, obs, method = minimize_metric,
                        metric = abs_d_sesp)
    expect_equal(optcut$optimal_cutpoint, 1)
    optcut <- cutpointr(exdat, preds, obs, method = maximize_metric,
                        metric = accuracy)
    expect_equal(optcut$optimal_cutpoint, 1)
    optcut <- cutpointr(exdat, preds, obs, method = maximize_metric,
                        metric = youden)
    expect_equal(optcut$optimal_cutpoint, 1)

    # With NA
    exdat <- data.frame(obs = c(NA, 0, 0, 1, 1),
                        preds = c(1, 0, 0, 1, 1))
    optcut <- cutpointr(exdat, preds, obs, method = minimize_metric,
                        metric = abs_d_sesp, na.rm = T)
    expect_equal(optcut$optimal_cutpoint, 1)
    expect_silent(plot(optcut))
    optcut <- cutpointr(exdat, preds, obs, method = maximize_metric,
                        metric = accuracy, na.rm = T)
    expect_equal(optcut$optimal_cutpoint, 1)
    expect_silent(plot(optcut))
    optcut <- cutpointr(exdat, preds, obs, method = maximize_metric,
                        metric = youden, na.rm = T)
    expect_equal(optcut$optimal_cutpoint, 1)
    expect_silent(plot(optcut))

    # With Inf and -Inf
    exdat <- data.frame(obs = c(rep(0, 20),
                                1, 1, 1, 1, 0, 0, 0, 0),
                        preds = c(rep(-Inf, 10),
                                  rep(Inf, 10),
                                  0, 0, 0, 0, 1, 1, 1, 1))
    optcut <- cutpointr(exdat, preds, obs,
                        method = maximize_metric, metric = cutpointr::youden)
    expect_equal(optcut$optimal_cutpoint, 1)
    expect_equal(optcut$specificity, 1)
    expect_equal(round(optcut$sensitivity, 2), 0.58)
})


test_that("Bad metric colnames are detected", {
    metricfunc <- function(tp, fp, tn, fn) {
        res <- matrix(1:length(tp), ncol = 1)
        colnames(res) <- "sensitivity"
        return(res)
    }
    expect_error(cutpointr(suicide, dsi, suicide, metric = metricfunc))

    metricfunc <- function(tp, fp, tn, fn) {
        res <- matrix(1:length(tp), ncol = 1)
        colnames(res) <- "AUC"
        return(res)
    }
    expect_error(cutpointr(elas, elas, status, metric = metricfunc))
})

test_that("SE and NSE interface give identical results", {
    opt_cut_nse <- cutpointr(suicide, dsi, suicide)
    opt_cut_se <- cutpointr_(suicide, "dsi", "suicide")
    expect_identical(opt_cut_se, opt_cut_nse)

    opt_cut_nse <- cutpointr(suicide, dsi, suicide)
    opt_cut_se <- cutpointr_(suicide, "dsi", "suicide",
                            method = "maximize_metric")
    expect_identical(opt_cut_se, opt_cut_nse)

    opt_cut_nse <- cutpointr(suicide, dsi, suicide, gender)
    opt_cut_se <- cutpointr_(suicide, "dsi", "suicide", "gender")
    expect_identical(opt_cut_se, opt_cut_nse)

    opt_cut_nse <- cutpointr(suicide, dsi, suicide, gender)
    opt_cut_se <- cutpointr_(suicide, "dsi", "suicide", "gender")
    expect_identical(opt_cut_se, opt_cut_nse)
})

test_that("cutpointr detects wrong number of classes", {
    tempdat <- data.frame(cl = factor(c("a", "b", "c")), x = 1:3)
    expect_error(cutpointr(tempdat, x, cl))
    expect_error(cutpointr(tempdat, "x", "cl"))
    expect_error(cutpointr(tempdat, x, cl, pos_class = "a", neg_class = "b",
                           direction = ">="))
    expect_error(cutpointr(tempdat, "x", "cl", pos_class = "a", neg_class = "b",
                           direction = ">="))

    tempdat <- data.frame(cl = factor(c("a", "a", "a")), x = 1:3)
    expect_error(cutpointr(tempdat, x, cl))
    expect_error(cutpointr(tempdat, "x", "cl"))
    expect_error(cutpointr(tempdat, x, cl, pos_class = "a", neg_class = "b",
                           direction = ">="))
    expect_error(cutpointr(tempdat, "x", "cl", pos_class = "a", neg_class = "b",
                           direction = ">="))
})