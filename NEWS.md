# jfsp 0.3.5

* Update `jfsp_plot` to offer optional argument to triplicate annual historical data for inter-annual variability in burn area and prepend to each projected RCP series.
* Updated data sets, making column names more consistent and informative.

# jfsp 0.3.0

* Added decadal `firesize` data set.
* Added fire size distributions plot to `jfsp_plot` type options.
* Refactored `jfsp_plot` to handle RCP marginalization and Treatment dropping via `by_rcp` and `by_tx`.
* Additional minor updates.

# jfsp 0.2.0

* Refactored `jfsp_plot`, making data sets implicit so that specifying them via argument is not necessary. Reduced, reordered formal arguments.
* Added optional arguments, including statewide aggregation of `fmoba` data for applicable plots using `alaska = TRUE`.

# jfsp 0.1.0

* Added primary plotting function for package data sets.
* Added seven package data sets.
* Update `jfsp_plot` function. It now handles plotting in R and saving plots to disk with an optional `file` argument. Some minimal handling for `showtext` issues when saving high dpi plots was also added.
* Updated unit tests.
* Updated documentation.

# jfsp 0.0.0.9000

* Initialized package and added packaged scaffolding.
