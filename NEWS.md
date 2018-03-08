# jfsp 0.6.0

* Added statewide Alaska burn area through time conditional on coniferous vs. deciduous tree species on the landscape.
* Updated documentation.

# jfsp 0.5.0

* Updated all data sets. Added `labels` and `shortDescription` attributes to variables in data frames for use in external data set documentation.
* Revised data prep script with function to assist in variable attribution.

# jfsp 0.4.0

* Updated several data sets.

# jfsp 0.3.5

* Updated `jfsp_plot` to offer optional argument to triplicate annual historical data for inter-annual variability in burn area and prepend to each projected RCP series.
* Added `pt_size` argument for controlling point size in applicable plot types.
* Made optional argument `n` for moving average plot applicable to all versions of plot. Now defaults to `n = 30`.
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
