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