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