# cutpointr 0.4.1

- Fix naming the metric in the bootstrap if the supplied function returns an
unnamed vector

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