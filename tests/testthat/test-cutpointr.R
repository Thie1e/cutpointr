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
    opt_cut <- suppressWarnings(cutpointr(suicide, dsi, suicide, boot_runs = 50))
    expect_true(mean(opt_cut$boot[[1]]$Sum_Sens_Spec_b) > 1.3 &
                    mean(opt_cut$boot[[1]]$Sum_Sens_Spec_b) < 3)
    expect_true(sd(opt_cut$boot[[1]]$Sum_Sens_Spec_b) > 0.02 &
                    sd(opt_cut$boot[[1]]$Sum_Sens_Spec_b) < 1)
    expect_true(mean(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) > 1.3 &
                    mean(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) < 3)
    expect_true(sd(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) > 0.02 &
                    sd(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) < 1)

    set.seed(123)
    opt_cut <- suppressWarnings(cutpointr_(suicide, "dsi", "suicide",
                                           boot_runs = 50))
    expect_true(mean(opt_cut$boot[[1]]$Sum_Sens_Spec_b) > 1.3 &
                    mean(opt_cut$boot[[1]]$Sum_Sens_Spec_b) < 3)
    expect_true(sd(opt_cut$boot[[1]]$Sum_Sens_Spec_b) > 0.02 &
                    sd(opt_cut$boot[[1]]$Sum_Sens_Spec_b) < 1)
    expect_true(mean(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) > 1.3 &
                    mean(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) < 3)
    expect_true(sd(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) > 0.02 &
                    sd(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) < 1)

    set.seed(123)
    opt_cut <- suppressWarnings(cutpointr(suicide, dsi, suicide, boot_runs = 30,
                         direction = "<="))
    expect_true(mean(opt_cut$boot[[1]]$Sum_Sens_Spec_b) > 1.3 &
                    mean(opt_cut$boot[[1]]$Sum_Sens_Spec_b) < 3)
    expect_true(sd(opt_cut$boot[[1]]$Sum_Sens_Spec_b) > 0.02 &
                    sd(opt_cut$boot[[1]]$Sum_Sens_Spec_b) < 1)
    expect_true(mean(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) > 1.3 &
                    mean(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) < 3)
    expect_true(sd(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) > 0.02 &
                    sd(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) < 1)

    set.seed(123)
    opt_cut <- suppressWarnings(cutpointr_(suicide, "dsi", "suicide",
                                           boot_runs = 30, direction = "<="))
    expect_true(mean(opt_cut$boot[[1]]$Sum_Sens_Spec_b) > 1.3 &
                    mean(opt_cut$boot[[1]]$Sum_Sens_Spec_b) < 3)
    expect_true(sd(opt_cut$boot[[1]]$Sum_Sens_Spec_b) > 0.02 &
                    sd(opt_cut$boot[[1]]$Sum_Sens_Spec_b) < 1)
    expect_true(mean(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) > 1.3 &
                    mean(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) < 3)
    expect_true(sd(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) > 0.02 &
                    sd(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) < 1)

    set.seed(123)
    opt_cut <- suppressWarnings(cutpointr(suicide, dsi, suicide, boot_runs = 30,
                         pos_class = "no"))
    expect_true(mean(opt_cut$boot[[1]]$Sum_Sens_Spec_b) > 1.3 &
                    mean(opt_cut$boot[[1]]$Sum_Sens_Spec_b) < 3)
    expect_true(sd(opt_cut$boot[[1]]$Sum_Sens_Spec_b) > 0.02 &
                    sd(opt_cut$boot[[1]]$Sum_Sens_Spec_b) < 1)
    expect_true(mean(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) > 1.3 &
                    mean(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) < 3)
    expect_true(sd(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) > 0.02 &
                    sd(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) < 1)

    set.seed(123)
    opt_cut <- suppressWarnings(cutpointr_(suicide, "dsi", "suicide",
                                           boot_runs = 30, pos_class = "no"))
    expect_true(mean(opt_cut$boot[[1]]$Sum_Sens_Spec_b) > 1.3 &
                    mean(opt_cut$boot[[1]]$Sum_Sens_Spec_b) < 3)
    expect_true(sd(opt_cut$boot[[1]]$Sum_Sens_Spec_b) > 0.02 &
                    sd(opt_cut$boot[[1]]$Sum_Sens_Spec_b) < 1)
    expect_true(mean(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) > 1.3 &
                    mean(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) < 3)
    expect_true(sd(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) > 0.02 &
                    sd(opt_cut$boot[[1]]$Sum_Sens_Spec_oob) < 1)
})

test_that("Summary by class returns correct stats", {
    # No subgroup, no NA
    oc <- cutpointr(suicide, dsi, suicide)
    s <- summary(oc)
    my <- mean(suicide[suicide$suicide == "yes", "dsi"])
    expect_equal(s[[1]]$desc_byclass["yes", "Mean"], my)
    mn <- mean(suicide[suicide$suicide == "no", "dsi"])
    expect_equal(s[[1]]$desc_byclass["no", "Mean"], mn)

    # No subgroup, with NA
    tempdat <- suicide
    tempdat[10, 1] <- NA
    tempdat[20, 2] <- NA
    tempdat[30, 3] <- NA
    tempdat[40, 4] <- NA
    oc <- cutpointr(tempdat, dsi, suicide, na.rm = TRUE)
    s <- summary(oc)
    my <- mean(tempdat[tempdat$suicide == "yes", "dsi"], na.rm = TRUE)
    expect_equal(s[[1]]$desc_byclass["yes", "Mean"], my)
    mn <- mean(tempdat[tempdat$suicide == "no", "dsi"], na.rm = TRUE)
    expect_equal(s[[1]]$desc_byclass["no", "Mean"], mn)

    # With subgroup, no NA
    oc <- cutpointr(suicide, dsi, suicide, gender)
    s <- summary(oc)
    myf <- mean(suicide[suicide$suicide == "yes" & suicide$gender == "female", "dsi"])
    expect_equal(s$female$desc_byclass["yes", "Mean"], myf)
    mnf <- mean(suicide[suicide$suicide == "no" & suicide$gender == "female", "dsi"])
    expect_equal(s$female$desc_byclass["no", "Mean"], mnf)
    mym <- mean(suicide[suicide$suicide == "yes" & suicide$gender == "male", "dsi"])
    expect_equal(s$male$desc_byclass["yes", "Mean"], mym)
    mnm <- mean(suicide[suicide$suicide == "no" & suicide$gender == "male", "dsi"])
    expect_equal(s$male$desc_byclass["no", "Mean"], mnm)

    # With subgroup, with NA
    tempdat <- suicide
    tempdat[10, 1] <- NA
    tempdat[20, 2] <- NA
    tempdat[30, 3] <- NA
    tempdat[40, 4] <- NA
    oc <- cutpointr(tempdat, dsi, suicide, gender, na.rm = TRUE)
    s <- summary(oc)
    myf <- mean(tempdat[tempdat$suicide == "yes" & tempdat$gender == "female", "dsi"], na.rm = TRUE)
    expect_equal(s$female$desc_byclass["yes", "Mean"], myf)
    mnf <- mean(tempdat[tempdat$suicide == "no" & tempdat$gender == "female", "dsi"], na.rm = TRUE)
    expect_equal(s$female$desc_byclass["no", "Mean"], mnf)
    mym <- mean(tempdat[tempdat$suicide == "yes" & tempdat$gender == "male", "dsi"], na.rm = TRUE)
    expect_equal(s$male$desc_byclass["yes", "Mean"], mym)
    mnm <- mean(tempdat[tempdat$suicide == "no" & tempdat$gender == "male", "dsi"], na.rm = TRUE)
    expect_equal(s$male$desc_byclass["no", "Mean"], mnm)
})

test_that("Results for youden are equal to results by OptimalCutpoints", {
    opt_cut_cp <- cutpointr(suicide, dsi, suicide, metric = youden)
    opt_cut_oc <- cutpointr(suicide, dsi, suicide,
                             method = oc_OptimalCutpoints, oc_metric = "Youden")
    expect_equal(opt_cut_cp$optimal_cutpoint, opt_cut_oc$optimal_cutpoint)

    opt_cut_cp <- cutpointr(suicide, dsi, suicide, gender, metric = youden)
    opt_cut_oc <- cutpointr(suicide, dsi, suicide, gender,
                             method = oc_OptimalCutpoints, oc_metric = "Youden")
    expect_equal(opt_cut_cp$optimal_cutpoint, opt_cut_oc$optimal_cutpoint)
})

test_that("Results for p_chisquared are equal to results by OptimalCutpoints", {
    set.seed(2839)
    tempdat <- data.frame(x = c(rnorm(50), rnorm(50, mean = 1)) ,
                          y = c(rep(0, 50), rep(1, 50)),
                          group = sample(c("a", "b"), size = 100, replace = TRUE))
    suppressWarnings(
        opt_cut_cp <- cutpointr(tempdat, x, y, method = minimize_metric,
                                metric = p_chisquared, direction = ">=",
                                pos_class = 1)
    )
    suppressWarnings(
        opt_cut_oc <- cutpointr(tempdat, x, y, method = oc_OptimalCutpoints,
                                oc_metric = "MinPvalue")
    )
    expect_equal(opt_cut_cp$optimal_cutpoint, opt_cut_oc$optimal_cutpoint)

    suppressWarnings(
        opt_cut_cp <- cutpointr(tempdat, x, y, group, method = minimize_metric,
                                metric = p_chisquared, direction = ">=",
                                pos_class = 1)
    )
    suppressWarnings(
        opt_cut_oc <- cutpointr(tempdat, x, y, group,
                                method = oc_OptimalCutpoints,
                                oc_metric = "MinPvalue")
    )
    expect_equal(opt_cut_cp$optimal_cutpoint, opt_cut_oc$optimal_cutpoint)
})

test_that("Results for prod_sens_spec are equal to results by OptimalCutpoints", {
    set.seed(839)
    tempdat <- data.frame(x = c(rnorm(50), rnorm(50, mean = 1)) ,
                          y = c(rep(0, 50), rep(1, 50)),
                          group = sample(c("a", "b"), size = 100, replace = TRUE))

    opt_cut_cp <- cutpointr(tempdat, x, y, method = maximize_metric,
                            metric = prod_sens_spec, direction = ">=",
                            pos_class = 1)
    opt_cut_oc <- cutpointr(tempdat, x, y, method = oc_OptimalCutpoints,
                            oc_metric = "MaxProdSpSe")
    expect_equal(opt_cut_cp$optimal_cutpoint, opt_cut_oc$optimal_cutpoint)

    opt_cut_cp <- cutpointr(tempdat, x, y, group, method = maximize_metric,
                            metric = prod_sens_spec, direction = ">=",
                            pos_class = 1)
    opt_cut_oc <- cutpointr(tempdat, x, y, group,
                            method = oc_OptimalCutpoints,
                            oc_metric = "MaxProdSpSe")
    expect_equal(opt_cut_cp$optimal_cutpoint, opt_cut_oc$optimal_cutpoint)
})

test_that("Results for abs_d_ppvnpv are equal to results by OptimalCutpoints", {
    set.seed(389)
    tempdat <- data.frame(x = c(rnorm(50), rnorm(50, mean = 1)) ,
                          y = c(rep(0, 50), rep(1, 50)),
                          group = sample(c("a", "b"), size = 100, replace = TRUE))

    opt_cut_cp <- cutpointr(tempdat, x, y, method = minimize_metric,
                            metric = abs_d_ppvnpv, direction = ">=",
                            pos_class = 1)
    opt_cut_oc <- cutpointr(tempdat, x, y, method = oc_OptimalCutpoints,
                            oc_metric = "NPVEqualPPV")
    expect_equal(opt_cut_cp$optimal_cutpoint, opt_cut_oc$optimal_cutpoint)

    opt_cut_cp <- cutpointr(tempdat, x, y, group, method = minimize_metric,
                            metric = abs_d_ppvnpv, direction = ">=",
                            pos_class = 1)
    opt_cut_oc <- cutpointr(tempdat, x, y, group,
                            method = oc_OptimalCutpoints,
                            oc_metric = "NPVEqualPPV")
    expect_equal(opt_cut_cp$optimal_cutpoint, opt_cut_oc$optimal_cutpoint)
})

test_that("Results for sum_ppvnpv are equal to results by OptimalCutpoints", {
    set.seed(389)
    tempdat <- data.frame(x = c(rnorm(50), rnorm(50, mean = 1)) ,
                          y = c(rep(0, 50), rep(1, 50)),
                          group = sample(c("a", "b"), size = 100, replace = TRUE))

    opt_cut_cp <- cutpointr(tempdat, x, y, method = maximize_metric,
                            metric = sum_ppvnpv, direction = ">=",
                            pos_class = 1)
    opt_cut_oc <- cutpointr(tempdat, x, y, method = oc_OptimalCutpoints,
                            oc_metric = "MaxSumNPVPPV")
    expect_equal(opt_cut_cp$optimal_cutpoint, opt_cut_oc$optimal_cutpoint)

    opt_cut_cp <- cutpointr(tempdat, x, y, group, method = maximize_metric,
                            metric = sum_ppvnpv, direction = ">=",
                            pos_class = 1)
    opt_cut_oc <- cutpointr(tempdat, x, y, group,
                            method = oc_OptimalCutpoints,
                            oc_metric = "MaxSumNPVPPV")
    expect_equal(opt_cut_cp$optimal_cutpoint, opt_cut_oc$optimal_cutpoint)
})

test_that("Results for prod_ppvnpv are equal to results by OptimalCutpoints", {
    set.seed(389)
    tempdat <- data.frame(x = c(rnorm(50), rnorm(50, mean = 1)) ,
                          y = c(rep(0, 50), rep(1, 50)),
                          group = sample(c("a", "b"), size = 100, replace = TRUE))

    opt_cut_cp <- cutpointr(tempdat, x, y, method = maximize_metric,
                            metric = prod_ppvnpv, direction = ">=",
                            pos_class = 1)
    opt_cut_oc <- cutpointr(tempdat, x, y, method = oc_OptimalCutpoints,
                            oc_metric = "MaxProdNPVPPV")
    expect_equal(opt_cut_cp$optimal_cutpoint, opt_cut_oc$optimal_cutpoint)

    opt_cut_cp <- cutpointr(tempdat, x, y, group, method = maximize_metric,
                            metric = prod_ppvnpv, direction = ">=",
                            pos_class = 1)
    opt_cut_oc <- cutpointr(tempdat, x, y, group,
                            method = oc_OptimalCutpoints,
                            oc_metric = "MaxProdNPVPPV")
    expect_equal(opt_cut_cp$optimal_cutpoint, opt_cut_oc$optimal_cutpoint)
})

test_that("Results for prod_ppvnpv are equal to results by OptimalCutpoints", {
    set.seed(389)
    tempdat <- data.frame(x = c(rnorm(50), rnorm(50, mean = 1)) ,
                          y = c(rep(0, 50), rep(1, 50)),
                          group = sample(c("a", "b"), size = 100, replace = TRUE))

    opt_cut_cp <- cutpointr(tempdat, x, y, method = maximize_metric,
                            metric = prod_ppvnpv, direction = ">=",
                            pos_class = 1)
    opt_cut_oc <- cutpointr(tempdat, x, y, method = oc_OptimalCutpoints,
                            oc_metric = "MaxProdNPVPPV")
    expect_equal(opt_cut_cp$optimal_cutpoint, opt_cut_oc$optimal_cutpoint)

    opt_cut_cp <- cutpointr(tempdat, x, y, group, method = maximize_metric,
                            metric = prod_ppvnpv, direction = ">=",
                            pos_class = 1)
    opt_cut_oc <- cutpointr(tempdat, x, y, group,
                            method = oc_OptimalCutpoints,
                            oc_metric = "MaxProdNPVPPV")
    expect_equal(opt_cut_cp$optimal_cutpoint, opt_cut_oc$optimal_cutpoint)
})

test_that("Results for accuracy are equal to results by OptimalCutpoints", {
    set.seed(38429)
    tempdat <- data.frame(x = c(rnorm(100), rnorm(100, mean = 1)) ,
                          y = c(rep(0, 100), rep(1, 100)),
                          group = sample(c("a", "b"), size = 200, replace = TRUE))

    opt_cut_cp <- cutpointr(tempdat, x, y, method = maximize_metric,
                            metric = accuracy, direction = ">=",
                            pos_class = 1)
    opt_cut_oc <- cutpointr(tempdat, x, y, method = oc_OptimalCutpoints,
                            oc_metric = "MaxEfficiency")
    expect_equal(opt_cut_cp$optimal_cutpoint, opt_cut_oc$optimal_cutpoint)

    opt_cut_cp <- cutpointr(tempdat, x, y, group, method = maximize_metric,
                            metric = accuracy, direction = ">=",
                            pos_class = 1)
    opt_cut_oc <- cutpointr(tempdat, x, y, group,
                            method = oc_OptimalCutpoints,
                            oc_metric = "MaxProdNPVPPV")
    expect_equal(opt_cut_cp$optimal_cutpoint, opt_cut_oc$optimal_cutpoint)
})

test_that("Results for F1_score are equal to results by ROCR", {
    set.seed(38429)
    tempdat <- data.frame(x = c(rnorm(100), rnorm(100, mean = 1)) ,
                          y = c(rep(0, 100), rep(1, 100)),
                          group = sample(c("a", "b"), size = 200, replace = TRUE))

    f1_cp <- cutpointr(tempdat, x, y, method = maximize_metric,
                            metric = F1_score, direction = ">=",
                            pos_class = 1)
    rocr_pred <- ROCR::prediction(tempdat$x, tempdat$y)
    f1_rocr <- ROCR::performance(rocr_pred, "f")
    expect_identical(round(f1_rocr@y.values[[1]], 4)[-1],
                     round(f1_cp$roc_curve[[1]]$m, 4)[-1])
})

if (require(OptimalCutpoints)) {
    test_that("Results for misclassification_cost are equal to results by OptimalCutpoints", {
        set.seed(429)
        tempdat <- data.frame(x = c(rnorm(100), rnorm(100, mean = 1)) ,
                              y = c(rep(0, 100), rep(1, 100)),
                              group = sample(c("a", "b"), size = 200, replace = TRUE))

        opt_cut_cp <- cutpointr(tempdat, x, y, method = minimize_metric,
                                metric = misclassification_cost, direction = ">=",
                                cost_fp = 1, cost_fn = 3,
                                pos_class = 1)
        oc_cont <- control.cutpoints(CFP = 1, CFN = 3)
        opt_cut_oc <- OptimalCutpoints::optimal.cutpoints(X = "x", status = "y",
                                                          method = "MCT",
                                                          tag.healthy = 0,
                                                          direction = "<",
                                                          data = tempdat,
                                                          control = oc_cont)
        opt_cut_oc <- opt_cut_oc$MCT$Global$optimal.cutoff$cutoff
        expect_equal(opt_cut_cp$optimal_cutpoint, opt_cut_oc)

        opt_cut_cp <- cutpointr(tempdat, x, y, group, method = minimize_metric,
                                metric = misclassification_cost, direction = ">=",
                                cost_fp = 1, cost_fn = 3,
                                pos_class = 1)
        oc_cont <- control.cutpoints(CFP = 1, CFN = 3)
        opt_cut_oc <- OptimalCutpoints::optimal.cutpoints(X = "x", status = "y",
                                                          categorical.cov = "group",
                                                          method = "MCT",
                                                          tag.healthy = 0,
                                                          direction = "<",
                                                          data = tempdat,
                                                          control = oc_cont)
        opt_cut_oc_a <- opt_cut_oc$MCT$a$optimal.cutoff$cutoff
        opt_cut_oc_b <- opt_cut_oc$MCT$b$optimal.cutoff$cutoff
        expect_equal(opt_cut_cp$optimal_cutpoint, c(opt_cut_oc_a, opt_cut_oc_b))
    })
}

test_that("LOESS smoothing does not return warnings or errors", {
    set.seed(38429)
    tempdat <- data.frame(x = c(rnorm(100), rnorm(100, mean = 1)) ,
                          y = c(rep(0, 100), rep(1, 100)),
                          group = sample(c("a", "b"), size = 200, replace = TRUE))

    expect_silent(cutpointr(tempdat, x, y, method = maximize_loess_metric,
                            user.span = 1,
                            metric = accuracy, direction = ">=",
                            pos_class = 1, boot_runs = 10))

    expect_silent(cutpointr(tempdat, x, y, group, method = maximize_loess_metric,
                            user.span = 1,
                            metric = accuracy, direction = ">=",
                            pos_class = 1, boot_runs = 10))
})

test_that("cutpointr returns same result with NSE interface and raw data", {
    oc1 <- cutpointr(suicide, dsi, suicide, metric = prod_sens_spec)
    oc2 <- cutpointr(x = suicide$dsi, class = suicide$suicide,
                     metric = prod_sens_spec)
    expect_true(oc1$optimal_cutpoint == 2)
    expect_true(oc2$optimal_cutpoint == 2)
    expect_true(oc1$Prod_Sens_Spec == oc2$Prod_Sens_Spec)

    oc1 <- cutpointr(suicide, dsi, suicide, gender, metric = prod_sens_spec)
    oc2 <- cutpointr(x = suicide$dsi, class = suicide$suicide,
                     subgroup = suicide$gender, metric = prod_sens_spec)
    expect_true(all(oc1$Prod_Sens_Spec == oc2$Prod_Sens_Spec))
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
