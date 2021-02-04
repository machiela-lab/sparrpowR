# sparrpowR (development version)

# sparrpowR 0.1.4

* Updated `spatstat` dependency to `spatstat.core` and `spatstat.geom`
* Changes to DESCRIPTION
  * Added ORCID for all authors and contributors
  * Fixed typos in Description
* Changes to `spatial_data()`
  * Used `as.solist()` to specify appropriate class to lists
* Changes to `spatial_power()`
  * Added `p_correct` arugment to apply a multiple testing correction
  * Fixed typos in documentation
  * Deprecated `cascon` argument (moved to `spatial_plots()`)
  * Deprecated `lower_tail` and `upper_tail` arguments and replaced with argument `alpha`. Estimates case-only power (lower tail) and case/control (lower and upper tail) and captures both outputs. Case-only clustering (`cascon = FALSE`) is a (lower-tail) one-tailed hypothesis test by default, case and control clustering (`cascon = TRUE`) is a two-tailed hypothesis test by default, .
  * Used `as.solist()` to specify appropriate class to lists
* Changes to `spatial_plots()`:
  * Added `cascon` argument to toggle the display of case-only power (lower tail) or case/control (lower and upper tail) output
  * Added `plot_title` argument to toggle the display of titles in plots. Changed the default title names. 
  * Removed main panel title in plots
  * Fixed default colors and removed mislabled 'midpoint' color
  * Removed annotation from `plot.ppp()` in plot #3 and plot #2 (if points are not plotted)
  * Added `scale` arguement to scale text for higher resolution plots
  * Added `horizontal` argument to toggle the display of the color key to be on the right (vertical) or bottom (horizontal) of plots
  * Added `plot_axes` argument to toggle the display of axes in plots
  * Added `plot_square` argument to toggle the margins of plots
  * Changed the value of `cex.axis` and `cex` in plots
* Changes to `jitter_power()`
  * Added `p_correct` arugment to apply a multiple testing correction
  * Specify all packages for functions
  * Deprecated `cascon` argument (moved to `spatial_plots()`)
  * Estimates case-only power (lower tail) and case/control (lower and upper tail) and captures both outputs. Case-only is now a one-tailed hypothesis test by default. Deprecated `lower_tail` and `upper_tail` arguments and replaced with argument `alpha`
  * Used `as.solist()` to specify appropriate class to lists
  * Updated example to reflect new updates
* Changes to vignette
  * Set global chunk options
  * Named code chunks
  * Increased the `sim_total` value for a more realistic example
  * Specify all packages for functions
  * Updated code to reflect updates to functions in `sparrpowR`
* Changes to README.md
  * Added CRAN badges
  * Added plotting for case and control clustering
* Added utils.R file
  * Moved an internal helper functions to a utils.R file
  * Removed this internal helper functions from `jitter_power()`, `spatial_power()`, and `spatial_plots()`
* Added zzz.R file
* Added package.R file
* Imports 'lifecycle' package to document deprecated argument `cascon` in `spatial_power()` and `jitter_power()` functions
* Removed 'parallel' package from Imports for parallel processing in `spatial_power()` and `jitter_power()` functions
* Removed 'tidyverse' package from Suggests 