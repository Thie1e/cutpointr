# cutpointr 1.1.2

## Changes
- The (very long) vignette has been split up into multiple vignettes, each
 with its own topic. @kapsner has written an additional vignette on 
 bootstrapping.
- Update ?multi_cutpointr to say that it uses cutpointr(), not cutpointr_()

## Fixes
- When using an extra parameter for the metric, such as in sens_constrain, and 
if bootstrapping was run, that parameter was not forwarded to the bootstrapping


# cutpointr 1.1.1

## Changes
- Clarified the help for the boot_stratify parameter
- Better examples for the constrained metrics
- Added reference and citation for the article about cutpointr in JSS

# cutpointr 1.1.0

## Changes
- `boot_ci` now works with multiple cutpoints (multiple cutpoints are possible
if `break_ties = c`).
- `add_metric` now adds the selected metrics to the bootstrap results, too.
- Include metrics that were added with `add_metric` in `summary()`.
- Change default value of `subgroup` in `multi_cutpointr` to `NULL` 
(instead of missing) to make it consistent with `cutpointr`.
- No rounding anymore within the internal function `summary_sd` so that the
various summary functions now return all values without rounding.
- Descriptive statistics in summary functions are now stored with the additional
first column "Data" instead of giving the class values as row names.
- Nicer printing of summary objects in Rmd documents.
- `boot_stratify` is now passed to the method functions so that the bootstrap
within `maximize_boot_metric` and `minimize_boot_metric` can be stratified, too.
- Subtitles (such as "by class") have been removed from the plots because 
the subtitles should have read "by subgroup" and because this is already clear
from the legend.

## Fixes
- Fix a bug in `multi_cutpointr` that forced the `class` variable to be
named "suicide". 

# cutpointr 1.0.32

- Reduce size of tarball for CRAN by removing some superfluous files

# cutpointr 1.0.31

- Accelerate tests and vignette building to lower runtime on CRAN

# cutpointr 1.0.3

- Some changes for vctrs 0.3.0 and dplyr 1.0.0: Unname and correctly assign
classes to a few objects instead of using vctrs::df_cast and vctrs::df_ptype2
to keep compatibility with vctrs 0.2.4
- Added ORCID and article reference to Description

# cutpointr 1.0.2

- Minor internal changes for compatibility with the latest updates of
some packages from the tidyverse (e.g. `tibble` 3.0.0)

# cutpointr 1.0.1

- Prepare for matrix inheriting from class "array" in R 4.0.0 by making a minor
change in non-exported utility function `sanitize_metric`

# cutpointr 1.0.0

## Most important changes
- `cutpointr` and `roc` now both use tidyeval. `!!` can be used when an argument
should be unquoted, as in `dplyr`, 
e.g. `myvar <- "dsi"; cutpointr(suicide, !!myvar, suicide)`. `cutpointr_` is now
deprecated. Transforming variables directly in the call is thus no longer
supported, e.g. `cutpointr(suicide, dsi * 2, suicide)` now throws an error.
- The object returned by `multi_cutpointr` does not have the `cutpointr` class
anymore. 

## New functions
- A new `boot_ci` function is available that calculates confidence intervals
(the empirical quantiles) based on the bootstrap results.
- The `auc` function is now exported and can be used to calculate the AUC from
a `cutpointr` or `roc_cutpointr` object, 
e.g. `auc(roc(suicide, dsi, suicide, "yes", "no"))`
- `boot_test` is a new function for carrying out a bootstrap test for 
equivalence of a metric, e.g. the AUC, the Youden-Index or also the optimal
cutpoint. The standard deviation is calculated as `sd` of the differences
in metric values per bootstrap repetition, then a z-test is calculated.

## Plotting
- New `type` argument to `plot_roc` for choosing line or step 
- The resulting object from `roc_cutpointr` can now simply be plotted with 
`plot()`

## Bootstrapping
- The bootstrapping no longer tries to redraw bootstrap samples if only one 
class is drawn. In that case the repetition is removed from the results via
`.errorhandling = "remove"` in `foreach`.
- Subsequently report the number of missing values in the bootstrap results. 
`summary.cutpointr` and `summary.multi_cutpointr` now print an
additional `NAs` column in the bootstrap summary
and `cutpointr` issues a message if any bootstrap repeats failed (e.g. because
only one class was drawn). 
- Stratified bootstrapping is now supported via the `boot_stratify` argument.

# Misc
- Make the printed output of `summary.cutpointr`  and `summary.multi_cutpointr`
more compact
- No rounding of numbers in `summary.cutpointr` and `summary.multi_cutpointr`
any more. The rounding is now done in `print.summary_cutpointr` and 
`print.summary_multi_cutpointr`, respectively, and can be controlled via the
`digits` argument
- `plot_metric` has a new `add_unsmoothed` argument for adding the unsmoothed
metric values to the plot as a dashed line (default `TRUE`). Helpful to 
inspect the smoothing of functions like `maximize_gam_metric`.
- Add some mathematical details to `?oc_youden_kernel`.
- The Readme and vignette have been updated and condensed a bit.
- A warning is issued if in `metric_constrain` or one of the other constrained
metrics `min_constrain` can not be achieved.

## Fixes
- Fix the default for `break_ties` in `cutpointr.default` by setting it to
`median` as it was already in `cutpointr.numeric` and `cutpointr_`.



# cutpointr 0.7.6
- Let `roc()` return a tibble instead of a data.frame
- Printing results of `roc()` is now possible with `plot_roc()`
- Extra metric columns can now be added to a `roc_cutpointr` object with `add_metric()`
- Add prostate_nodal data set of nodal involvement and acid phosphatase levels
in 53 prostate cancer patients
- Fix fetching of method name if method was called using `::` or `:::`
- Make test of summary printing more tolerant after problems with `tidyr` 0.8.3
- Issue an error if plot is used on a `multi_cutpointr` object
- Add a summary method for `multi_cutpointr`, a corresponding 
`summary_multi_cutpointr` class and a printing method for that class
- The column `variable` is not returned anymore by `multi_cutpointr`, because
it is identical to `predictor`
- Run `multi_cutpointr` only on all numeric columns, if `x = NULL`

# cutpointr 0.7.5
- Add constrainable metrics, e.g. sens_constrain to calculate sensitivity given
a minimum value for specificity
- Fix a bug where dot-arguments were not passed to the metric function in
cutpointr_internal
- Add a check to ensure that the metric function does not return only missing
values
- Replace (fix) ">" by ">=" in the documentation of `cutpointr()`.

# cutpointr 0.7.4
- Add `sigfig` argument to `print.cutpointr` to allow for specifying the number of
significant digits to be printed
- Add `add_metric()` function to add further metrics to the output of `cutpointr()`
- Add `roc01` metric function to calculate the distance of points on the ROC
curve to the point (0,1) on ROC space
- Fix `plot_sensitivity_specificity()` if `boot_runs = 0`

# cutpointr 0.7.3
- Fix display of bootstrap results in summary
- Update benchmarks in Readme

# cutpointr 0.7.2
- More tests and modified two tests that led to errors when CRAN checked 
the package (`spar = NULL` in `maximize_spline_metric`)
- Add links to the documentation of all method, plotting, main and metric functions 
to all other functions of the same family
- Add some checks to predict.cutpointr to prevent improper `cutpoint_nr`
- The `boot` column is now always returned and `NA`, if no bootstrapping was
run, so that the number of returned columns is constant

# cutpointr 0.7.1
- Fix a bug in check_method_cols that occurred when a user-supplied method 
function returned a metric column but no roc_curve
- Add chapters to the Readme about GAM, kernel and normal methods
- Fix bug in bootstrapping that allowed resamples with only one class to be 
sampled
- `use_midpoints` is now also passed to `method` by `cutpointr` to allow for
the calculation of midpoints within `maximize_boot_metric` and `minimize_boot_metric`,
which before happened in `cutpointr`, leading to slightly biased cutpoints
in certain scenarios

# cutpointr 0.7.0
- Change defaults in spline smoothing: `nknots` is now calculated by 
`stats::.nknots_smspl` and `spar = 1`
- Add `cutpoint_tol` argument to define a tolerance around the optimized metric,
so that multiple cutpoints in the vicinity of the target metric can be returned
and to avoid not returning other "optimal" cutpoints due to floating-point
problems
- Change default value to `break_ties = c`
- If multiple optimal cutpoints are found and they are summarized using 
`break_ties`, the returned main metric is now not the optimal one but the
one corresponding to the summarized cutpoint (thus may be worse than the 
optimal one)
- Add `maximize_gam_metric` and `minimize_gam_metric` for smoothing via
generalized additive models
- All plots with `geom_ribbon` now use `size = 0` to plot no lines around the
(transparent) areas

# cutpointr 0.6.0
- Return multiple optimal cutpoints
- Add spline smoothing method

# cutpointr 0.5.2
- Reformat docs 
- Remove superfluous check in `plot_cutpointr`
- Add metrics `plr` (positive likelihood ratio), `nlr` (negative likelihood ratio),
`false_discovery_rate`, and `false_omission_rate`

# cutpointr 0.5.1

- In preparation for a new version of tibble that will restore the old printing
behaviour and in order to pass the CRAN check the print method for cutpointr
has been altered depending on the loaded version of tibble.
- `silent` argument for roc().
- Reformat docs to include better formatted equations
- Remove superfluous check in plot_cutpointr

# cutpointr 0.5.0

- `cutpointr_` now accepts functions instead of character strings as `method` 
or `metric`
- Fix naming the metric in the bootstrap if the supplied function returns an
unnamed vector
- Fix help for the `use_midpoints` parameter. If TRUE (default FALSE) the
returned optimal cutpoint will be the mean of the optimal cutpoint and the next
lowest observation (for `direction = ">="`) or the next highest observation
(for `direction = "<="`)
- Rename `sum_ppvnpv`, `prod_ppvnpv`, and `abs_d_ppvnpv` to `sum_ppv_npv`,
`prod_ppv_npv`, and `abs_d_ppvnpv` to match the naming scheme to the names of 
the metrics that optimize sensitivity and specificity
- Also all metric names that are returned by the metric functions are now
lower case
- The `summary_sd` function now also returns 5% and 95% percentiles that are
included in the output of `summary`
- The default number of bootstrap repeats in `minimize_boot_metric` and 
`maximize_boot_metric` was changed from 200 to 50
- The message "Running bootstrap..." is now displayed before executing the
bootstrap so that possible messages or warnings can be attributed to the main
cutpoint estimation or cutpoint estimation during the bootstrapping
- The `summary` function now returns a data.frame instead of a list, also
the printing method for `summary_cutpointr` has been slightly modified
- Add `plot_sensitivity_specificity` for plotting cutpoints vs. 
sensitivity and specificity on the y-axis
- Remove `oc_optimalCutpoints` function
- Remove dependencies `ROCR` and `OptimalCutpoints` by rewriting tests and 
storing benchmark results

# cutpointr 0.4.0

- cutpointr() now also works on vectors of raw data, that is without the `data`
argument. Thus, it can be used as before by specifying `data`, `x`, and `class`
or alternatively without specifying `data` and directly supplying the vectors
of predictions and outcomes as `x` and `class`.
- `silent` argument for optionally suppressing messages (e.g. which class
is assumed to be the positive one)
- Parts of the code were rewritten using Rcpp which leads to roughly a threefold
speedup with larger n (n > 1e5)
- The automatic determination of the positive / negative classes and direction
now uses the median of the predictor values per class instead of the mean so 
that the AUC is always >= 0.5
- The bootstrapping now also returns the in-bag main metric, the out-of-bag 
ROC curve, and the out-of-bag AUC


# cutpointr 0.3.2

- Add multi_cutpointr for running cutpointr_ on multiple predictor variables
- Add metric risk_ratio
- Use geom_bar instead of geom_histogram if all cutpoints are integer in plot_x and plot_cut_boot
or if there is just one unique value
- If bootstrapping is run, a bootstrapped confidence interval can be displayed 
in plot_metric
- Add methods for median and mean as cutpoint
- Fix display of cutpoint in ROC curve (use get_opt_ind in plot.cutpoint and plot_roc)
- Fix picking correct metric in plot_metric_boot