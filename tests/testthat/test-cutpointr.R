test_that("Cutpointr returns a cutpointr without NAs and a certain Nr of rows", {
    library(OptimalCutpoints)
    data(elas)
    opt_cut <- cutpointr(elas, elas, status)
    expect_true("cutpointr" %in% class(opt_cut))
    expect_that(nrow(opt_cut), equals(1))
    expect_that(sum(is.na(opt_cut)), equals(0))
})