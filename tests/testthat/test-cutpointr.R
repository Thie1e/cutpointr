test_that("Cutpointr returns a cutpointr without NAs and a certain Nr of rows", {
    data(suicide)
    opt_cut <- cutpointr(suicide, dsi, suicide)
    expect_true("cutpointr" %in% class(opt_cut))
    expect_that(nrow(opt_cut), equals(1))
    expect_that(sum(is.na(opt_cut)), equals(1)) # boot is NA
    expect_silent(plot(opt_cut))
    expect_silent(plot_metric(opt_cut))
    expect_silent(plot_roc(opt_cut))
    expect_silent(plot_x(opt_cut))
    expect_silent(plot_precision_recall(opt_cut))
    expect_silent(plot_sensitivity_specificity(opt_cut))
})

test_that("Cutpointr works with different data types", {
    set.seed(456)
    tempdat <- data.frame(x = rnorm(10),
                          y = sample(0:1, size = 10, replace = TRUE))
    opt_cut <- cutpointr(tempdat, x, y)
    expect_that(nrow(opt_cut), equals(1))
    expect_that(sum(is.na(opt_cut)), equals(1))
    expect_silent(plot(opt_cut))

    tempdat$y <- factor(tempdat$y)
    opt_cut <- cutpointr(tempdat, x, y)
    expect_that(nrow(opt_cut), equals(1))
    expect_that(sum(is.na(opt_cut)), equals(1))
    expect_silent(plot(opt_cut))

    tempdat$y <- as.character(tempdat$y)
    opt_cut <- cutpointr(tempdat, x, y)
    expect_that(nrow(opt_cut), equals(1))
    expect_that(sum(is.na(opt_cut)), equals(1))
    expect_silent(plot(opt_cut))

    # With subgroup
    set.seed(567)
    tempdat <- data.frame(x = rnorm(30),
                          y = sample(0:1, size = 30, replace = TRUE),
                          g = sample(0:2, size = 30, replace = TRUE))
    opt_cut <- cutpointr(tempdat, x, y, g)
    expect_that(nrow(opt_cut), equals(3))
    expect_that(sum(is.na(opt_cut)), equals(3))
    expect_silent(plot(opt_cut))
    expect_silent(plot_metric(opt_cut))
    expect_silent(plot_roc(opt_cut))
    expect_silent(plot_x(opt_cut))
    expect_silent(plot_precision_recall(opt_cut))
    expect_silent(plot_sensitivity_specificity(opt_cut))

    tempdat$g <- factor(tempdat$g)
    opt_cut <- cutpointr(tempdat, x, y, g)
    expect_that(nrow(opt_cut), equals(3))
    expect_that(sum(is.na(opt_cut)), equals(3))
    expect_silent(plot(opt_cut))
    expect_silent(plot_metric(opt_cut))
    expect_silent(plot_roc(opt_cut))
    expect_silent(plot_x(opt_cut))
    expect_silent(plot_precision_recall(opt_cut))
    expect_silent(plot_sensitivity_specificity(opt_cut))

    tempdat$g <- as.character(tempdat$g)
    opt_cut <- cutpointr(tempdat, x, y, g)
    expect_that(nrow(opt_cut), equals(3))
    expect_that(sum(is.na(opt_cut)), equals(3))
    expect_silent(plot(opt_cut))
    expect_silent(plot_metric(opt_cut))
    expect_silent(plot_roc(opt_cut))
    expect_silent(plot_x(opt_cut))
    expect_silent(plot_precision_recall(opt_cut))
    expect_silent(plot_sensitivity_specificity(opt_cut))
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

    set.seed(102)
    opt_cut <- cutpointr(suicide, dsi, suicide, gender, method = minimize_metric,
                         metric = abs_d_sens_spec, boot_runs = 50, silent = TRUE)
    expect_silent(plot_cut_boot(opt_cut))
    expect_silent(plot_metric(opt_cut, conf_lvl = 0.9))
    expect_silent(plot_metric_boot(opt_cut))
    expect_silent(plot_precision_recall(opt_cut))
    expect_silent(plot_sensitivity_specificity(opt_cut))
    expect_silent(plot_roc(opt_cut))
})

test_that("AUC calculation is correct and works with Inf and -Inf", {
    tempdat <- data.frame(x = c(-Inf, 0.3, Inf),
                          y = factor(c(0, 1, 1)))
    roc_cutpointr <- cutpointr::roc(tempdat, "x", "y", pos_class = 1, neg_class = 0)
    auc_cutpointr <- cutpointr:::auc(roc_cutpointr$tpr, roc_cutpointr$fpr)
    expect_equal(auc_cutpointr, 1)
    cp <- cutpointr(tempdat, x, y)
    expect_equal(cp$AUC, 1)

    set.seed(123)
    tempdat <- data.frame(x = runif(100),
                          y = factor(sample(0:1, size = 100, replace = TRUE)))
    roc_cutpointr <- cutpointr::roc(tempdat, "x", "y", pos_class = 1, neg_class = 0)
    auc_cutpointr <- cutpointr:::auc(roc_cutpointr$tpr, roc_cutpointr$fpr)
    expect_equal(auc_cutpointr, 0.428)
    cp <- cutpointr(tempdat, x, y, pos_class = 1, direction = ">=")
    expect_equal(cp$AUC, 0.428)
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
    expect_equal(cutpointr:::find_metric_name(optcut), "youden_index")
    set.seed(1234)
    tempdat <- data.frame(x = runif(100),
                          y = factor(sample(0:1, size = 100, replace = TRUE)),
                          g = factor(sample(0:1, size = 100, replace = TRUE)))
    optcut <- cutpointr(tempdat, x, y, g, method = maximize_metric, metric = youden)
    expect_equal(cutpointr:::find_metric_name(optcut), "youden_index")
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
})

test_that("Correct cutpoints with example data", {
    exdat <- data.frame(obs = c(0, 0, 1, 1),
                        preds = c(0, 0, 1, 1))
    optcut <- cutpointr(exdat, preds, obs, method = minimize_metric,
                        metric = abs_d_sens_spec)
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
    expect_error(cutpointr(exdat, preds, obs, method = minimize_metric,
                           metric = abs_d_sens_spec))
    optcut <- cutpointr(exdat, preds, obs, method = minimize_metric,
                        metric = abs_d_sens_spec, na.rm = T)
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


test_that("Metric colnames that are already in cutpointr are modified", {
    metricfunc <- function(tp, fp, tn, fn) {
        res <- matrix(1:length(tp), ncol = 1)
        colnames(res) <- "sensitivity"
        return(res)
    }
    opt_cut <- cutpointr(suicide, dsi, suicide, metric = metricfunc,
                         boot_runs = 5)
    expect_equal(colnames(opt_cut)[4], "metric_sensitivity")
    expect_silent(plot(opt_cut))
    expect_silent(summary(opt_cut))

    opt_cut <- cutpointr(suicide, dsi, suicide, gender, metric = metricfunc,
                         boot_runs = 5)
    expect_equal(colnames(opt_cut)[5], "metric_sensitivity")
    expect_silent(plot(opt_cut))
    expect_silent(summary(opt_cut))

    metricfunc <- function(tp, fp, tn, fn) {
        res <- matrix(1:length(tp), ncol = 1)
        colnames(res) <- "AUC"
        return(res)
    }
    opt_cut <- cutpointr(suicide, dsi, suicide, metric = metricfunc,
                         boot_runs = 5)
    expect_equal(colnames(opt_cut)[4], "metric_AUC")
    expect_silent(plot(opt_cut))
    expect_silent(summary(opt_cut))
    opt_cut <- cutpointr(suicide, dsi, suicide, gender, metric = metricfunc,
                         boot_runs = 5)
    expect_equal(colnames(opt_cut)[5], "metric_AUC")
    expect_silent(plot(opt_cut))
    expect_silent(summary(opt_cut))

    metricfunc <- function(tp, fp, tn, fn) {
        res <- matrix(1:length(tp), ncol = 1)
        colnames(res) <- "roc_curve"
        return(res)
    }
    expect_error(cutpointr(suicide, dsi, suicide, metric = metricfunc,
                         boot_runs = 5))
    expect_error(cutpointr(suicide, dsi, suicide, gender, metric = metricfunc,
                         boot_runs = 5))
    opt_cut <- cutpointr(suicide, dsi, suicide, method = oc_youden_normal,
                         metric = metricfunc, boot_runs = 5)
    expect_equal(colnames(opt_cut)[4], "metric_roc_curve")
    expect_silent(plot(opt_cut))
    expect_silent(summary(opt_cut))

    opt_cut <- cutpointr(suicide, dsi, suicide, gender, method = oc_youden_normal,
                         metric = metricfunc, boot_runs = 5)
    expect_equal(colnames(opt_cut)[5], "metric_roc_curve")
    expect_silent(plot(opt_cut))
    expect_silent(summary(opt_cut))
})

test_that("SE and NSE interface give identical results", {
    opt_cut_nse <- cutpointr(suicide, dsi, suicide)
    opt_cut_se <- cutpointr_(suicide, "dsi", "suicide")
    expect_identical(opt_cut_se, opt_cut_nse)

    opt_cut_nse <- cutpointr(suicide, dsi, suicide)
    opt_cut_se <- cutpointr_(suicide, "dsi", "suicide",
                            method = maximize_metric)
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
    expect_error(cutpointr_(tempdat, "x", "cl"))
    expect_error(cutpointr(tempdat, x, cl, pos_class = "a", neg_class = "b",
                           direction = ">="))
    expect_error(cutpointr_(tempdat, "x", "cl", pos_class = "a", neg_class = "b",
                           direction = ">="))

    tempdat <- data.frame(cl = factor(c("a", "a", "a")), x = 1:3)
    expect_error(cutpointr(tempdat, x, cl))
    expect_error(cutpointr_(tempdat, "x", "cl"))
    expect_error(cutpointr(tempdat, x, cl, pos_class = "a", neg_class = "b",
                           direction = ">="))
    expect_error(cutpointr_(tempdat, "x", "cl", pos_class = "a", neg_class = "b",
                           direction = ">="))
})

test_that("Bootstrap returns plausible results", {
    set.seed(123)
    opt_cut <- suppressWarnings(cutpointr(suicide, dsi, suicide,
                                          boot_runs = 50, break_ties = mean))
    expect_true(mean(opt_cut$boot[[1]]$sum_sens_spec_b) > 1.3 &
                    mean(opt_cut$boot[[1]]$sum_sens_spec_b) < 3)
    expect_true(sd(opt_cut$boot[[1]]$sum_sens_spec_b) > 0.02 &
                    sd(opt_cut$boot[[1]]$sum_sens_spec_b) < 1)
    expect_true(mean(opt_cut$boot[[1]]$sum_sens_spec_oob) > 1.3 &
                    mean(opt_cut$boot[[1]]$sum_sens_spec_oob) < 3)
    expect_true(sd(opt_cut$boot[[1]]$sum_sens_spec_oob) > 0.02 &
                    sd(opt_cut$boot[[1]]$sum_sens_spec_oob) < 1)

    set.seed(123)
    opt_cut <- suppressWarnings(cutpointr_(suicide, "dsi", "suicide",
                                           boot_runs = 50, break_ties = mean))
    expect_true(mean(opt_cut$boot[[1]]$sum_sens_spec_b) > 1.3 &
                    mean(opt_cut$boot[[1]]$sum_sens_spec_b) < 3)
    expect_true(sd(opt_cut$boot[[1]]$sum_sens_spec_b) > 0.02 &
                    sd(opt_cut$boot[[1]]$sum_sens_spec_b) < 1)
    expect_true(mean(opt_cut$boot[[1]]$sum_sens_spec_oob) > 1.3 &
                    mean(opt_cut$boot[[1]]$sum_sens_spec_oob) < 3)
    expect_true(sd(opt_cut$boot[[1]]$sum_sens_spec_oob) > 0.02 &
                    sd(opt_cut$boot[[1]]$sum_sens_spec_oob) < 1)

    set.seed(123)
    opt_cut <- suppressWarnings(cutpointr(suicide, dsi, suicide, boot_runs = 30,
                         direction = "<="))
    expect_true(mean(opt_cut$boot[[1]]$sum_sens_spec_b) > 1.3 &
                    mean(opt_cut$boot[[1]]$sum_sens_spec_b) < 3)
    expect_true(sd(opt_cut$boot[[1]]$sum_sens_spec_b) > 0.02 &
                    sd(opt_cut$boot[[1]]$sum_sens_spec_b) < 1)
    expect_true(mean(opt_cut$boot[[1]]$sum_sens_spec_oob) > 1.3 &
                    mean(opt_cut$boot[[1]]$sum_sens_spec_oob) < 3)
    expect_true(sd(opt_cut$boot[[1]]$sum_sens_spec_oob) > 0.02 &
                    sd(opt_cut$boot[[1]]$sum_sens_spec_oob) < 1)

    set.seed(123)
    opt_cut <- suppressWarnings(cutpointr_(suicide, "dsi", "suicide",
                                           boot_runs = 30, direction = "<="))
    expect_true(mean(opt_cut$boot[[1]]$sum_sens_spec_b) > 1.3 &
                    mean(opt_cut$boot[[1]]$sum_sens_spec_b) < 3)
    expect_true(sd(opt_cut$boot[[1]]$sum_sens_spec_b) > 0.02 &
                    sd(opt_cut$boot[[1]]$sum_sens_spec_b) < 1)
    expect_true(mean(opt_cut$boot[[1]]$sum_sens_spec_oob) > 1.3 &
                    mean(opt_cut$boot[[1]]$sum_sens_spec_oob) < 3)
    expect_true(sd(opt_cut$boot[[1]]$sum_sens_spec_oob) > 0.02 &
                    sd(opt_cut$boot[[1]]$sum_sens_spec_oob) < 1)

    set.seed(123)
    opt_cut <- suppressWarnings(cutpointr(suicide, dsi, suicide, boot_runs = 30,
                         pos_class = "no"))
    expect_true(mean(opt_cut$boot[[1]]$sum_sens_spec_b) > 1.3 &
                    mean(opt_cut$boot[[1]]$sum_sens_spec_b) < 3)
    expect_true(sd(opt_cut$boot[[1]]$sum_sens_spec_b) > 0.02 &
                    sd(opt_cut$boot[[1]]$sum_sens_spec_b) < 1)
    expect_true(mean(opt_cut$boot[[1]]$sum_sens_spec_oob) > 1.3 &
                    mean(opt_cut$boot[[1]]$sum_sens_spec_oob) < 3)
    expect_true(sd(opt_cut$boot[[1]]$sum_sens_spec_oob) > 0.02 &
                    sd(opt_cut$boot[[1]]$sum_sens_spec_oob) < 1)

    set.seed(123)
    opt_cut <- suppressWarnings(cutpointr_(suicide, "dsi", "suicide",
                                           boot_runs = 30, pos_class = "no"))
    expect_true(mean(opt_cut$boot[[1]]$sum_sens_spec_b) > 1.3 &
                    mean(opt_cut$boot[[1]]$sum_sens_spec_b) < 3)
    expect_true(sd(opt_cut$boot[[1]]$sum_sens_spec_b) > 0.02 &
                    sd(opt_cut$boot[[1]]$sum_sens_spec_b) < 1)
    expect_true(mean(opt_cut$boot[[1]]$sum_sens_spec_oob) > 1.3 &
                    mean(opt_cut$boot[[1]]$sum_sens_spec_oob) < 3)
    expect_true(sd(opt_cut$boot[[1]]$sum_sens_spec_oob) > 0.02 &
                    sd(opt_cut$boot[[1]]$sum_sens_spec_oob) < 1)
})

test_that("Summary by class returns correct stats", {
    # No subgroup, no NA
    oc <- cutpointr(suicide, dsi, suicide)
    s <- summary(oc)
    my <- round(mean(suicide[suicide$suicide == "yes", "dsi"]), 4)
    expect_equal(s$desc_by_class[[1]]["yes", "Mean"], my)
    mn <- round(mean(suicide[suicide$suicide == "no", "dsi"]), 4)
    expect_equal(s$desc_by_class[[1]]["no", "Mean"], mn)

    # No subgroup, with NA
    tempdat <- suicide
    tempdat[10, 1] <- NA
    tempdat[20, 2] <- NA
    tempdat[30, 3] <- NA
    tempdat[40, 4] <- NA
    oc <- cutpointr(tempdat, dsi, suicide, na.rm = TRUE)
    s <- summary(oc)
    my <- round(mean(tempdat[tempdat$suicide == "yes", "dsi"], na.rm = TRUE), 4)
    expect_equal(s$desc_by_class[[1]]["yes", "Mean"], my)
    mn <- round(mean(tempdat[tempdat$suicide == "no", "dsi"], na.rm = TRUE), 4)
    expect_equal(s$desc_by_class[[1]]["no", "Mean"], mn)

    # With subgroup, no NA
    oc <- cutpointr(suicide, dsi, suicide, gender)
    s <- summary(oc)
    myf <- round(mean(suicide[suicide$suicide == "yes" & suicide$gender == "female", "dsi"]), 4)
    expect_equal(s$desc_by_class[[1]]["yes", "Mean"], myf)
    mnf <- round(mean(suicide[suicide$suicide == "no" & suicide$gender == "female", "dsi"]), 4)
    expect_equal(s$desc_by_class[[1]]["no", "Mean"], mnf)
    mym <- round(mean(suicide[suicide$suicide == "yes" & suicide$gender == "male", "dsi"]), 4)
    expect_equal(s$desc_by_class[[2]]["yes", "Mean"], mym)
    mnm <- round(mean(suicide[suicide$suicide == "no" & suicide$gender == "male", "dsi"]), 4)
    expect_equal(s$desc_by_class[[2]]["no", "Mean"], mnm)

    # With subgroup, with NA
    tempdat <- suicide
    tempdat[10, 1] <- NA
    tempdat[20, 2] <- NA
    tempdat[30, 3] <- NA
    tempdat[40, 4] <- NA
    oc <- cutpointr(tempdat, dsi, suicide, gender, na.rm = TRUE)
    s <- summary(oc)
    myf <- round(mean(tempdat[tempdat$suicide == "yes" & tempdat$gender == "female", "dsi"], na.rm = TRUE), 4)
    expect_equal(s$desc_by_class[[1]]["yes", "Mean"], myf)
    mnf <- round(mean(tempdat[tempdat$suicide == "no" & tempdat$gender == "female", "dsi"], na.rm = TRUE), 4)
    expect_equal(s$desc_by_class[[1]]["no", "Mean"], mnf)
    mym <- round(mean(tempdat[tempdat$suicide == "yes" & tempdat$gender == "male", "dsi"], na.rm = TRUE), 4)
    expect_equal(s$desc_by_class[[2]]["yes", "Mean"], mym)
    mnm <- round(mean(tempdat[tempdat$suicide == "no" & tempdat$gender == "male", "dsi"], na.rm = TRUE), 4)
    expect_equal(s$desc_by_class[[2]]["no", "Mean"], mnm)
})

test_that("Results for youden are correct", {
    opt_cut_cp <- cutpointr(suicide, dsi, suicide, metric = youden)
    expect_equal(opt_cut_cp$optimal_cutpoint, 2)

    opt_cut_cp <- cutpointr(suicide, dsi, suicide, gender, metric = youden)
    expect_equal(opt_cut_cp$optimal_cutpoint, c(2, 3))
})

test_that("Results for p_chisquared are equal to results by OptimalCutpoints", {
    set.seed(2839)
    tempdat <- data.frame(x = c(rnorm(50), rnorm(50, mean = 1)) ,
                          y = c(rep(0, 50), rep(1, 50)),
                          group = sample(c("a", "b"), size = 100, replace = TRUE))
    suppressWarnings(
        opt_cut_cp <- cutpointr(tempdat, x, y, method = minimize_metric,
                                metric = p_chisquared, direction = ">=",
                                pos_class = 1, tol_metric = 0)
    )
    expect_equal(round(opt_cut_cp$optimal_cutpoint, 4), 0.9335)

    suppressWarnings(
        opt_cut_cp <- cutpointr(tempdat, x, y, group, method = minimize_metric,
                                metric = p_chisquared, direction = ">=",
                                pos_class = 1, tol_metric = 0)
    )
    expect_equal(round(opt_cut_cp$optimal_cutpoint, 4), c(0.9335, 0.5676))
})

test_that("Results for prod_sens_spec are equal to results by OptimalCutpoints", {
    set.seed(839)
    tempdat <- data.frame(x = c(rnorm(50), rnorm(50, mean = 1)) ,
                          y = c(rep(0, 50), rep(1, 50)),
                          group = sample(c("a", "b"), size = 100, replace = TRUE))

    opt_cut_cp <- cutpointr(tempdat, x, y, method = maximize_metric,
                            metric = prod_sens_spec, direction = ">=",
                            pos_class = 1)
    expect_equal(round(opt_cut_cp$optimal_cutpoint, 4), 0.7783)

    opt_cut_cp <- cutpointr(tempdat, x, y, group, method = maximize_metric,
                            metric = prod_sens_spec, direction = ">=",
                            pos_class = 1)
    expect_equal(round(opt_cut_cp$optimal_cutpoint, 4), c(0.7998, 0.6371))
})

test_that("Results for abs_d_ppv_npv are equal to results by OptimalCutpoints", {
    set.seed(389)
    tempdat <- data.frame(x = c(rnorm(50), rnorm(50, mean = 1)) ,
                          y = c(rep(0, 50), rep(1, 50)),
                          group = sample(c("a", "b"), size = 100, replace = TRUE))

    opt_cut_cp <- cutpointr(tempdat, x, y, method = minimize_metric,
                            metric = abs_d_ppv_npv, direction = ">=",
                            pos_class = 1)
    expect_equal(round(opt_cut_cp$optimal_cutpoint, 4), 0.5677)

    opt_cut_cp <- cutpointr(tempdat, x, y, group, method = minimize_metric,
                            metric = abs_d_ppv_npv, direction = ">=",
                            pos_class = 1)
    expect_equal(round(opt_cut_cp$optimal_cutpoint, 4), c(0.2501, 0.6781))
})

test_that("Results for sum_ppv_npv are equal to results by OptimalCutpoints", {
    set.seed(389)
    tempdat <- data.frame(x = c(rnorm(50), rnorm(50, mean = 1)) ,
                          y = c(rep(0, 50), rep(1, 50)),
                          group = sample(c("a", "b"), size = 100, replace = TRUE))

    opt_cut_cp <- cutpointr(tempdat, x, y, method = maximize_metric,
                            metric = sum_ppv_npv, direction = ">=",
                            pos_class = 1)
    expect_equal(round(opt_cut_cp$optimal_cutpoint, 4), 1.7835)

    opt_cut_cp <- cutpointr(tempdat, x, y, group, method = maximize_metric,
                            metric = sum_ppv_npv, direction = ">=",
                            pos_class = 1)
    expect_equal(round(opt_cut_cp$optimal_cutpoint, 4), c(-0.7339, 1.7835))
})

test_that("Results for prod_ppv_npv are equal to results by OptimalCutpoints", {
    set.seed(389)
    tempdat <- data.frame(x = c(rnorm(50), rnorm(50, mean = 1)) ,
                          y = c(rep(0, 50), rep(1, 50)),
                          group = sample(c("a", "b"), size = 100, replace = TRUE))

    opt_cut_cp <- cutpointr(tempdat, x, y, method = maximize_metric,
                            metric = prod_ppv_npv, direction = ">=",
                            pos_class = 1)
    expect_equal(round(opt_cut_cp$optimal_cutpoint, 4), 1.7835)

    opt_cut_cp <- cutpointr(tempdat, x, y, group, method = maximize_metric,
                            metric = prod_ppv_npv, direction = ">=",
                            pos_class = 1)
    expect_equal(round(opt_cut_cp$optimal_cutpoint, 4), c(-0.7339, 1.7835))
})

test_that("Results for accuracy are equal to results by OptimalCutpoints", {
    set.seed(38429)
    tempdat <- data.frame(x = c(rnorm(100), rnorm(100, mean = 1)) ,
                          y = c(rep(0, 100), rep(1, 100)),
                          group = sample(c("a", "b"), size = 200, replace = TRUE))

    opt_cut_cp <- cutpointr(tempdat, x, y, method = maximize_metric,
                            metric = accuracy, direction = ">=",
                            pos_class = 1)
    expect_equal(round(opt_cut_cp$optimal_cutpoint, 4), 0.4312)

    opt_cut_cp <- cutpointr(tempdat, x, y, group, method = maximize_metric,
                            metric = accuracy, direction = ">=",
                            pos_class = 1)
    expect_equal(round(opt_cut_cp$optimal_cutpoint, 4), c(0.9771, 0.0744))
})

test_that("Results for F1_score are equal to results by ROCR", {
    set.seed(38429)
    tempdat <- data.frame(x = c(rnorm(100), rnorm(100, mean = 1)) ,
                          y = c(rep(0, 100), rep(1, 100)),
                          group = sample(c("a", "b"), size = 200, replace = TRUE))

    f1_cp <- cutpointr(tempdat, x, y, method = maximize_metric,
                            metric = F1_score, direction = ">=",
                            pos_class = 1)
    # rocr_pred <- ROCR::prediction(tempdat$x, tempdat$y)
    # f1_rocr <- ROCR::performance(rocr_pred, "f")
    # f1_rocr_yvals <- round(f1_rocr@y.values[[1]], 4)
    f1_rocr_yvals <- c(NaN, 0.0198, 0.0392, 0.0583, 0.0769, 0.0952, 0.1132,
                       0.1308, 0.1481, 0.1651, 0.1818, 0.1802, 0.1964, 0.2124, 0.2281, 0.2435,
                       0.2586, 0.2735, 0.2881, 0.3025, 0.3167, 0.3140, 0.3279, 0.3252, 0.3387,
                       0.3520, 0.3651, 0.3780, 0.3906, 0.3876, 0.4000, 0.4122,
                       0.4242, 0.4211, 0.4328, 0.4444, 0.4412, 0.4526, 0.4493, 0.4604, 0.4714,
                       0.4823, 0.4789, 0.4895, 0.5000, 0.5103, 0.5205, 0.5306,
                       0.5405, 0.5503, 0.5600, 0.5563, 0.5658, 0.5621, 0.5714, 0.5806,
                       0.5897, 0.5860, 0.5949, 0.6038, 0.6000, 0.5963, 0.6049, 0.6135,
                       0.6098, 0.6182, 0.6145, 0.6228, 0.6310, 0.6391, 0.6353, 0.6433,
                       0.6395, 0.6474, 0.6437, 0.6400, 0.6364, 0.6441, 0.6404, 0.6480,
                       0.6444, 0.6409, 0.6374, 0.6448, 0.6522, 0.6486, 0.6559, 0.6524,
                       0.6489, 0.6561, 0.6526, 0.6597, 0.6667, 0.6736, 0.6804, 0.6872,
                       0.6837, 0.6802, 0.6869, 0.6935, 0.7000, 0.6965, 0.7030, 0.7094,
                       0.7059, 0.7024, 0.6990, 0.7053, 0.7019, 0.6986, 0.6952, 0.7014,
                       0.7075, 0.7136, 0.7103, 0.7163, 0.7130, 0.7097, 0.7156, 0.7123,
                       0.7091, 0.7149, 0.7207, 0.7175, 0.7232, 0.7289, 0.7257, 0.7225,
                       0.7193, 0.7162, 0.7130, 0.7100, 0.7069, 0.7124, 0.7179, 0.7234,
                       0.7288, 0.7257, 0.7227, 0.7197, 0.7250, 0.7220, 0.7190, 0.7160,
                       0.7131, 0.7184, 0.7154, 0.7126, 0.7097, 0.7149, 0.7120, 0.7092,
                       0.7063, 0.7115, 0.7087, 0.7059, 0.7109, 0.7082, 0.7132, 0.7104,
                       0.7077, 0.7050, 0.7023, 0.6996, 0.6970, 0.7019, 0.6992, 0.7041,
                       0.7015, 0.7063, 0.7037, 0.7011, 0.7059, 0.7033, 0.7007, 0.6982,
                       0.6957, 0.6931, 0.6978, 0.6953, 0.6929, 0.6904, 0.6879, 0.6855,
                       0.6901, 0.6877, 0.6853, 0.6829, 0.6875, 0.6851, 0.6828, 0.6804,
                       0.6781, 0.6758, 0.6735, 0.6780, 0.6757, 0.6734, 0.6711, 0.6689, 0.6667)
    expect_identical(f1_rocr_yvals[-1], round(f1_cp$roc_curve[[1]]$m, 4)[-1])
})

test_that("Results for misclassification_cost are equal to results by OptimalCutpoints", {
    set.seed(429)
    tempdat <- data.frame(x = c(rnorm(100), rnorm(100, mean = 1)) ,
                          y = c(rep(0, 100), rep(1, 100)),
                          group = sample(c("a", "b"), size = 200, replace = TRUE))

    opt_cut_cp <- cutpointr(tempdat, x, y, method = minimize_metric,
                            metric = misclassification_cost, direction = ">=",
                            cost_fp = 1, cost_fn = 3,
                            pos_class = 1)
    # oc_cont <- control.cutpoints(CFP = 1, CFN = 3)
    # opt_cut_oc <- OptimalCutpoints::optimal.cutpoints(X = "x", status = "y",
    #                                                   method = "MCT",
    #                                                   tag.healthy = 0,
    #                                                   direction = "<",
    #                                                   data = tempdat,
    #                                                   control = oc_cont)
    # opt_cut_oc <- opt_cut_oc$MCT$Global$optimal.cutoff$cutoff
    expect_equal(round(opt_cut_cp$optimal_cutpoint, 4), -0.2973)

    opt_cut_cp <- cutpointr(tempdat, x, y, group, method = minimize_metric,
                            metric = misclassification_cost, direction = ">=",
                            cost_fp = 1, cost_fn = 3,
                            pos_class = 1)
    # oc_cont <- control.cutpoints(CFP = 1, CFN = 3)
    # opt_cut_oc <- OptimalCutpoints::optimal.cutpoints(X = "x", status = "y",
    #                                                   categorical.cov = "group",
    #                                                   method = "MCT",
    #                                                   tag.healthy = 0,
    #                                                   direction = "<",
    #                                                   data = tempdat,
    #                                                   control = oc_cont)
    # opt_cut_oc_a <- opt_cut_oc$MCT$a$optimal.cutoff$cutoff
    # opt_cut_oc_b <- opt_cut_oc$MCT$b$optimal.cutoff$cutoff
    expect_equal(round(opt_cut_cp$optimal_cutpoint, 4), c(-0.7498, -0.0824))
})

test_that("LOESS smoothing does not return warnings or errors", {
    set.seed(38429)
    tempdat <- data.frame(x = c(rnorm(100), rnorm(100, mean = 1)) ,
                          y = c(rep(0, 100), rep(1, 100)),
                          group = sample(c("a", "b"), size = 200, replace = TRUE))

    expect_silent(
        suppressMessages(
            cp <- cutpointr(tempdat, x, y, method = maximize_loess_metric,
                            user.span = 1,
                            metric = accuracy, direction = ">=",
                            pos_class = 1, boot_runs = 10)
        )
    )
    expect_equal(round(cp$optimal_cutpoint, 3), 0.507)
    expect_equal(round(cp$loess_accuracy, 3), 0.690)

    set.seed(208)
    tempdat <- data.frame(x = c(rnorm(100), rnorm(100, mean = 2)) ,
                          y = c(rep(0, 100), rep(1, 100)),
                          group = sample(c("a", "b"), size = 200, replace = TRUE))
    expect_silent(
        suppressMessages(
            cp <- cutpointr(tempdat, x, y, method = maximize_loess_metric,
                            metric = accuracy, direction = ">=",
                            pos_class = 1, boot_runs = 10)
        )
    )

    expect_silent(
        suppressMessages(
            cutpointr(tempdat, x, y, group, method = maximize_loess_metric,
                            user.span = 1,
                            metric = accuracy, direction = ">=",
                            pos_class = 1, boot_runs = 10)
        )
    )

    set.seed(3429)
    tempdat <- data.frame(x = c(rnorm(100), rnorm(100, mean = 1)) ,
                          y = c(rep(0, 100), rep(1, 100)),
                          group = sample(c("a", "b"), size = 200, replace = TRUE))

    expect_silent(
        suppressMessages(
            cp <- cutpointr(tempdat, x, y, method = minimize_loess_metric,
                            user.span = 1, break_ties = mean,
                            metric = abs_d_ppv_npv, direction = ">=",
                            pos_class = 1, boot_runs = 10)
        )
    )
    expect_equal(round(cp$optimal_cutpoint, 3), -0.083)
    expect_equal(round(cp$loess_abs_d_ppv_npv, 3), 0.156)

    expect_silent(
        cp <- cutpointr(tempdat, x, y, group, method = minimize_loess_metric,
                        user.span = 1, break_ties = mean, silent = TRUE,
                        metric = abs_d_ppv_npv, direction = ">=",
                        pos_class = 1, boot_runs = 100)
    )
    expect_equal(round(cp$optimal_cutpoint, 2), c(-1.29, 1.01))
})

test_that("cutpointr returns same result with NSE interface and raw data", {
    oc1 <- cutpointr(suicide, dsi, suicide, metric = prod_sens_spec)
    oc2 <- cutpointr(x = suicide$dsi, class = suicide$suicide,
                     metric = prod_sens_spec)
    expect_true(oc1$optimal_cutpoint == 2)
    expect_true(oc2$optimal_cutpoint == 2)
    expect_true(oc1$prod_sens_spec == oc2$prod_sens_spec)

    oc1 <- cutpointr(suicide, dsi, suicide, gender, metric = prod_sens_spec)
    oc2 <- cutpointr(x = suicide$dsi, class = suicide$suicide,
                     subgroup = suicide$gender, metric = prod_sens_spec)
    expect_true(all(oc1$prod_sens_spec == oc2$prod_sens_spec))
    expect_true(all(oc1$optimal_cutpoint == oc2$optimal_cutpoint))
})

test_that("Prevalence is correctly calculated", {
    tempdat <- data.frame(x = 1:100,
                          y = c(rep(0, 10), rep(1, 90)))
    oc <- cutpointr(tempdat, x, y, pos_class = 1, direction = ">=")
    expect_equal(oc$prevalence, 0.9)

    tempdat <- data.frame(x = 100:1,
                          y = c(rep(0, 10), rep(1, 90)))
    oc <- cutpointr(tempdat, x, y, pos_class = 1, direction = ">=")
    expect_equal(oc$prevalence, 0.9)

    tempdat <- data.frame(x = 1:100,
                          y = c(rep(0, 10), rep(1, 90)))
    oc <- cutpointr(tempdat, x, y, pos_class = 1, direction = "<=")
    expect_equal(oc$prevalence, 0.9)

    tempdat <- data.frame(x = 100:1,
                          y = c(rep(0, 10), rep(1, 90)))
    oc <- cutpointr(tempdat, x, y, pos_class = 1, direction = "<=")
    expect_equal(oc$prevalence, 0.9)
})

test_that("multi_cutpointr runs without errors", {
    mc <- multi_cutpointr(suicide, x = c("age", "dsi"), class = "suicide",
                          pos_class = "yes")
    expect_equal(mc$optimal_cutpoint, c(55, 2))

    mc <- multi_cutpointr(suicide, x = c("age", "dsi"), class = "suicide",
                          subgroup = "gender", pos_class = "yes")
    expect_equal(mc$optimal_cutpoint, c(55, 21, 2, 3))
})

test_that("AUC is always >= 0.5 with automatic assumptions", {
    tempdat <- data.frame(x = c(5:1, 100), y = c(2,2,2,1,1,1))
    oc <- cutpointr(tempdat, x, y)
    expect_true(oc$AUC >= 0.5)

    oc <- cutpointr(tempdat, x, y, pos_class = 2)
    expect_true(oc$AUC >= 0.5)
})

test_that("find_metric_name_boot finds correct metric", {
    oc <- cutpointr(x = suicide$dsi, class = suicide$suicide,
                    metric = abs_d_sens_spec, method = minimize_metric,
                    boot_runs = 10)
    expect_true(cutpointr:::find_metric_name_boot(oc$boot[[1]]) == "abs_d_sens_spec_oob")

    oc <- cutpointr(x = suicide$dsi, class = suicide$suicide,
                    subgroup = suicide$gender,
                    metric = abs_d_sens_spec, method = minimize_metric,
                    boot_runs = 10)
    expect_true(cutpointr:::find_metric_name_boot(oc$boot[[1]]) == "abs_d_sens_spec_oob")
})

test_that("error if NSE and raw interface are mixed", {
    expect_error(cutpointr(suicide, suicide$dsi, suicide$suicide))
    expect_error(cutpointr(suicide, suicide$dsi, suicide))
    expect_error(cutpointr(suicide, dsi, suicide$suicide))
    expect_error(cutpointr(suicide, dsi, suicide, suicide$gender))
})

test_that("error if metric/method is not a function", {
    expect_error(cutpointr(suicide, dsi, suicide, metric = "accuracy"))
    expect_error(cutpointr(suicide, dsi, suicide, method = "oc_youden_kernel"))
    expect_error(cutpointr_(suicide, "dsi", "suicide", metric = "accuracy"))
    expect_error(cutpointr_(suicide, "dsi", "suicide", method = "oc_youden_kernel"))
})

test_that("same results with or without silent", {
    expect_identical(
        cutpointr(suicide, dsi, suicide, gender),
        cutpointr(suicide, dsi, suicide, gender, silent = TRUE)
    )
    expect_identical(
        cutpointr_(suicide, "dsi", "suicide", "gender"),
        cutpointr_(suicide, "dsi", "suicide", "gender", silent = TRUE)
    )
})

test_that("plot_cutpointr runs", {
    oc <- cutpointr(suicide, dsi, suicide)
    expect_is(plot_cutpointr(oc), "ggplot")
    expect_is(plot_cutpointr(oc, cutpoints, fpr), "ggplot")
    expect_is(plot_cutpointr(oc, cutpoints, function(tp, fp, ...) tp + fp), "ggplot")

    oc <- cutpointr(suicide, dsi, suicide, gender)
    expect_is(plot_cutpointr(oc), "ggplot")
    expect_is(plot_cutpointr(oc, cutpoints, fpr), "ggplot")
    expect_is(plot_cutpointr(oc, cutpoints, function(tp, fp, ...) tp + fp), "ggplot")

    set.seed(100)
    oc <- cutpointr(suicide, dsi, suicide, boot_runs = 5)
    expect_is(plot_cutpointr(oc), "ggplot")
    expect_is(plot_cutpointr(oc, cutpoints, fpr), "ggplot")
    expect_is(plot_cutpointr(oc, cutpoints, function(tp, fp, ...) tp + fp), "ggplot")

    set.seed(100)
    oc <- cutpointr(suicide, dsi, suicide, gender, boot_runs = 5)
    expect_is(plot_cutpointr(oc), "ggplot")
    expect_is(plot_cutpointr(oc, cutpoints, fpr), "ggplot")
    expect_is(plot_cutpointr(oc, cutpoints, function(tp, fp, ...) tp + fp), "ggplot")

    set.seed(100)
    expect_warning(plot_cutpointr(
        cutpointr(suicide, dsi, suicide, boot_runs = 5), fpr, tpr
    ))
    set.seed(200)
    expect_warning(plot_cutpointr(
        cutpointr(suicide, dsi, suicide, gender, boot_runs = 5), fpr, tpr
    ))
})

test_that("smoothing splines lead to plausible results", {
    cp <- cutpointr(suicide, dsi, suicide, method = maximize_spline_metric,
                    nknots = 5, spar = 0.3)
    expect_equal(cp$optimal_cutpoint, 3)
    expect_silent(plot(cp))
    expect_silent(plot_metric(cp))
    expect_silent(plot_roc(cp))
    expect_silent(summary(cp))

    cp <- cutpointr(suicide, dsi, suicide, gender, method = maximize_spline_metric,
                    nknots = 5, spar = 0.3)
    expect_equal(cp$optimal_cutpoint, c(3, 3))
    expect_silent(plot(cp))
    expect_silent(plot_metric(cp))
    expect_silent(plot_roc(cp))
    expect_silent(summary(cp))

    cp <- cutpointr(suicide, dsi, suicide, method = minimize_spline_metric,
                    nknots = 5, spar = 0.3, df = 5, metric = abs_d_sens_spec)
    expect_equal(cp$optimal_cutpoint, 3)
    expect_silent(plot(cp))
    expect_silent(plot_metric(cp))
    expect_silent(plot_roc(cp))
    expect_silent(summary(cp))
})

test_that("gam smoothing leads to plausible results", {
    cp <- cutpointr(suicide, dsi, suicide, method = maximize_gam_metric,
                    metric = youden)
    expect_equal(cp$optimal_cutpoint, 2)
    expect_silent(plot(cp))
    expect_silent(plot_metric(cp))
    expect_silent(plot_roc(cp))
    expect_silent(summary(cp))

    cp <- cutpointr(suicide, dsi, suicide, gender, method = maximize_gam_metric,
                    metric = youden)
    expect_equal(cp$optimal_cutpoint, c(2, 2))
    expect_silent(plot(cp))
    expect_silent(plot_metric(cp))
    expect_silent(plot_roc(cp))
    expect_silent(summary(cp))

    cp <- cutpointr(suicide, dsi, suicide, gender, method = minimize_gam_metric,
                    metric = abs_d_sens_spec)
    expect_equal(cp$optimal_cutpoint, c(2, 2))
    expect_silent(plot(cp))
    expect_silent(plot_metric(cp))
    expect_silent(plot_roc(cp))
    expect_silent(summary(cp))
})

test_that("bootstrapped cutpoints lead to plausible results", {
    set.seed(914)
    cp <- cutpointr(suicide, dsi, suicide, method = maximize_boot_metric,
                    metric = youden, boot_cut = 10)
    expect_equal(cp$optimal_cutpoint, 1.9)
    expect_silent(plot(cp))
    expect_silent(plot_roc(cp))
    expect_silent(summary(cp))

    set.seed(14)
    cp <- cutpointr(suicide, dsi, suicide, gender, method = maximize_boot_metric,
                    metric = youden, boot_cut = 10)
    expect_equal(cp$optimal_cutpoint, c(2.2, 3.2))
    expect_silent(plot(cp))
    expect_silent(plot_roc(cp))
    expect_silent(summary(cp))

    set.seed(15)
    cp <- cutpointr(suicide, dsi, suicide, gender, method = minimize_boot_metric,
                    metric = abs_d_sens_spec, boot_cut = 10)
    expect_equal(cp$optimal_cutpoint, c(2.1, 2.2))
    expect_silent(plot(cp))
    expect_error(plot_metric(cp))
    expect_silent(plot_roc(cp))
    expect_silent(summary(cp))
})

test_that("this led to an error with get_rev_dups Rcpp function", {
    dat <- rbind(data.frame(x = round(rnorm(5000), 1), y = 0),
                 data.frame(x = round(rnorm(5000, mean = 0.05), 1), y = 1))
    expect_silent(cutpointr(dat, x, y, method = maximize_spline_metric,
                            nknots = 50, silent = TRUE))
})


test_that("cutpoint_knots returns correct results", {
    expect_equal(cutpoint_knots(suicide, "dsi"), 12)
})

test_that("cutpointr handles multiple optimal cutpoints correctly", {
    tempdat <- data.frame(y = c(0,0,0,1,0,1,1,1),
                          x = 1:8)
    expect_message(cp <- cutpointr(tempdat, x = x, class = y, break_ties = c,
                                   pos_class = 1, direction = ">="))
    expect_equal(cp$optimal_cutpoint[[1]], c(6, 4))
    expect_message(cp <- cutpointr(tempdat, x = x, class = y, break_ties = c,
                                   use_midpoints = TRUE, pos_class = 1,
                                   direction = ">="))
    expect_equal(cp$optimal_cutpoint[[1]], c(5.5, 3.5))

    tempdat_g <- data.frame(g = c(rep(1, 8), rep(2, 8)),
                            y = c(tempdat$y, tempdat$y),
                            x = c(tempdat$x, tempdat$x + 2))
    expect_message(cp <- cutpointr(tempdat_g, x = x, class = y, pos_class = 1,
                                   direction = ">=", subgroup = g, break_ties = c))
    preds <- predict(object = cp,
                     newdata = data.frame(x = c(3,6,7,1), g = c(1,2,2,1)),
                     cutpoint_nr = 2)
    expect_equal(preds, c(0,1,1,0))
    preds <- predict(object = cp,
                     newdata = data.frame(x = c(3,4,5,8), g = c(1,1,2,2)),
                     cutpoint_nr = c(2, 1))
    expect_equal(preds, c(0,1,0,1))

    # 5 "optimal" cutpoints at 1.2. Only one is returned without
    # using a tolerance argument due to floating point problems
    dat <-
        structure(list(x = c(107.163316194991, 105.577309820546, 114.819340158769,
                             93.8701224510515, 111.161366154904, 110.365480099412, 98.3751686809715,
                             90.2407330717812, 89.1085480911548, 104.57786957945, 99.2887326627911,
                             117.791026668514, 105.351379604962, 96.280551248365, 89.7445775150289,
                             100.175983253947, 109.428883928973, 101.490653529433, 111.142301202307,
                             102.656619478302, 104.944400912055, 98.6949032719217, 125.050435849087,
                             109.326217314704, 108.306336404995, 89.0813758511593, 112.597918995493,
                             95.7637641112029, 97.084784256457, 115.183411710216),
                       group = structure(c(1L,
                                           1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L,
                                           2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L),
                                         .Label = c("h", "d"), class = "factor")),
                  .Names = c("x", "group"), row.names = c(NA, -30L), class = "data.frame")
    cp <- cutpointr(dat, x, group, break_ties = c, tol_metric = 1e-6)
    expect_equal(length(unlist(cp$optimal_cutpoint)), 5)
})

test_that("Main metric gets replaced correctly when ties are broken", {
    dat <- structure(list(x = c(101.805229018197, 107.847340401023, 86.4683542621282,
                                119.832982062599, 112.384717044928, 112.006173961394, 108.961498842131,
                                102.536897550745, 105.496003408587, 119.033710510161, 124.445336141903,
                                111.152957581359, 103.727459196182, 97.3051126961894, 107.721798530394,
                                107.3951172956, 94.2585671912768, 127.544243110669, 93.2168880188317,
                                115.311444925151), group = structure(c(1L, 1L, 1L, 1L, 1L, 1L,
                                                                       1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c("h",
                                                                                                                                           "d"), class = "factor")), .Names = c("x", "group"), row.names = c(NA,
                                                                                                                                                                                                             -20L), class = "data.frame")
    cp <- cutpointr(dat, x, group, break_ties = c)
    cp2 <- cutpointr(dat, x, group, break_ties = mean)
    expect_equal(unlist(cp$sum_sens_spec), c(1.2, 1.2))
    expect_equal(cp2$sum_sens_spec, 1.1)

    # Different metric
    cp <- cutpointr(dat, x, group, metric = accuracy,
                    method = maximize_metric, break_ties = c)
    cp2 <- cutpointr(dat, x, group, metric = accuracy,
                    method = maximize_metric, break_ties = mean)
    expect_equal(unlist(cp$accuracy), c(0.6, 0.6))
    expect_equal(unlist(cp$accuracy), unlist(cp$acc))
    expect_equal(cp2$accuracy, 0.55)
    expect_equal(cp2$accuracy, cp2$acc)

    # With subgroup
    dat <- structure(list(x = c(112.154869479653, 85.0195562661719, 93.9648809281475,
                                111.629388719907, 93.6448243724487, 117.357328029692, 98.3663682555138,
                                105.201729160301, 98.7939462917362, 103.013183787135, 106.178862160569,
                                108.635928791856, 93.964291812696, 99.9357423611922, 106.763495052307,
                                114.17384726262, 127.593415952213, 95.1459299909085, 124.619049508866,
                                103.578770674126, 118.606425125718, 117.882345070528, 113.320440921296,
                                115.780191821353, 99.4794449460106, 115.080791705234, 104.429126735958,
                                119.686460888845, 106.942508474107, 119.110377570583, 121.929559539621,
                                121.503911629438, 116.669635368185, 106.99542786452, 106.089271831433,
                                119.975572786775, 120.126938685093, 98.1659607850261, 110.128131439393,
                                108.365631379854), group = structure(c(1L, 1L, 1L, 1L, 1L, 1L,
                                                                       1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L,
                                                                       2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
                                                                       2L, 2L), .Label = c("h", "d"), class = "factor"), subgroup = c(0L,
                                                                                                                                      1L, 0L, 1L, 0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 0L,
                                                                                                                                      0L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 1L, 0L, 1L,
                                                                                                                                      0L, 0L, 0L, 0L, 0L, 1L, 0L)), .Names = c("x", "group", "subgroup"
                                                                                                                                      ), row.names = c(NA, -40L), class = "data.frame")
    cp <- cutpointr(dat, x, group, subgroup, break_ties = c)
    cp2 <- cutpointr(dat, x, group, subgroup, break_ties = function(x) mean(x) - 10)
    expect_equal(round(cp$optimal_cutpoint[[2]], 4), c(113.3204, 110.1281))
    expect_equal(round(cp$sum_sens_spec[[2]], 3), rep(1.667, 2))
    expect_equal(round(cp2$optimal_cutpoint[[2]], 2), 101.72)
    expect_equal(round(cp2$sum_sens_spec[[2]], 3), 1.222)
})

test_that("simple_boot stops if no sets with both classes can be found", {
    dat <- data.frame(x = rnorm(50), y = 0)
    expect_error(cutpointr:::simple_boot(data = dat, dep_var = "y"))

    dat <- data.frame(x = rnorm(50), y = factor("a"))
    expect_error(cutpointr:::simple_boot(data = dat, dep_var = "y"))
})

test_that("cutpointr works with custom method function", {
    CutOff_Optimised <- function(data, x, class, pos_class,
                                 neg_class, direction, ...){

        stopifnot(direction == ">=")
        Obs <- data[[class]] == pos_class
        Fit <- data[[x]]
        SumObs <- sum(Obs)
        LengObs <- length(Obs)
        tt <- c(0)
        Cut <- c(0,0,0)

        if(length(unique(Fit))==1){
            Cut[1] <- unique(Fit)
            Cut[2] <- 100*sum((Fit>=Cut[1])[Obs==1])/SumObs
            Cut[3] <- 100*sum((Fit<Cut[1])[Obs==0])/(LengObs-SumObs)
            Cut <- t(Cut)
        }

        else{
            if(min(Fit)<0) Fit[Fit<0] <- 0
            Quant <- quantile(Fit)
            i <- Quant[1]
            a <- 2
            while(i<=Quant[5]){
                se <- sum((Fit>=i)[Obs==1])/SumObs
                sp <- sum((Fit<i)[Obs==0])/(LengObs-SumObs)
                tt[a] <- se+sp-1
                if(tt[a]<tt[a-1]) break
                i <- i+((Quant[5] - Quant[1])/1000)
                a <- a+1
            }
            b <- (i-((Quant[5] - Quant[1])/1000))
            Cut[1] <- b
            se <- sum((Fit>=b)[Obs==1])/SumObs
            sp <- sum((Fit<b)[Obs==0])/(LengObs-SumObs)
            Cut[2] <- se
            Cut[3] <- sp
            Cut <- t(Cut)
            dimnames(Cut)=list(NULL, c("CutOff", "se", "sp"))
        }
        return(data.frame(optimal_cutpoint = b, youden_index = se + sp - 1))
    }

    cp <- cutpointr(suicide, dsi, suicide, method = CutOff_Optimised)
    expect_equal(colnames(cp)[4], "youden_index")
    expect_equal(cp$optimal_cutpoint, 1.991)
    expect_equal(cp$method, "CutOff_Optimised")
    expect_silent(plot(cp))

    set.seed(927)
    cp <- cutpointr(suicide, dsi, suicide, method = CutOff_Optimised,
                    boot_runs = 10)
    expect_equal(colnames(cp)[4], "youden_index")
    expect_equal(cp$optimal_cutpoint, 1.991)
    expect_equal(cp$method, "CutOff_Optimised")
    expect_silent(plot(cp))

    cp <- cutpointr(suicide, dsi, suicide, gender, method = CutOff_Optimised)
    expect_equal(colnames(cp)[5], "youden_index")
    expect_equal(cp$optimal_cutpoint, c(1.990, 2.992))
    expect_equal(cp$method, c("CutOff_Optimised", "CutOff_Optimised"))
    expect_silent(plot(cp))

    set.seed(264)
    cp <- cutpointr(suicide, dsi, suicide, gender, method = CutOff_Optimised,
                    boot_runs = 10)
    expect_equal(colnames(cp)[5], "youden_index")
    expect_equal(cp$optimal_cutpoint, c(1.990, 2.992))
    expect_equal(cp$method, c("CutOff_Optimised", "CutOff_Optimised"))
    expect_silent(plot(cp))
})

test_that("predict behaves as expected", {
    cp <- cutpointr(suicide, dsi, suicide, gender)
    # Cutpoint 2 does not exist
    expect_error(predict(cp,
                         newdata = data.frame(dsi = c(2,3,4,5),
                                              gender = c("female", "female", "female", "male")),
                         cutpoint_nr = c(1,2)))

    cp <- cutpointr(suicide, dsi, suicide,
                    method = maximize_spline_metric, spar = 0.6)
    # Cutpoint 2 does not exist
    expect_error(predict(cp,
                         newdata = data.frame(dsi = c(2,3,4,5),
                                              gender = c("female", "female", "female", "male")),
                         cutpoint_nr = c(2)))
    # Subgroup does not exist
    expect_error(predict(cp,
                         newdata = data.frame(dsi = c(2,3,4,5),
                                              gender = c("female", "female", "female", "male")),
                         cutpoint_nr = c(1, 1)))

    expect_equal(predict(cp, newdata = data.frame(dsi = 1:5)),
                 factor(c("no", "no", "yes", "yes", "yes")))

    cp <- cutpointr(suicide, dsi, suicide, use_midpoints = TRUE,
                    method = maximize_spline_metric, spar = 0.6)
    expect_equal(predict(cp, newdata = data.frame(dsi = 1:5)),
                 factor(c("no", "no", "yes", "yes", "yes")))
})

