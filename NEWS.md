# sparrpowR (development version)

# sparrpowR 0.1.4.9000

* Updated `spatstat` dependency to `spatstat.core`
* Changes to DESCRIPTION
  * Added ORCID for all authors and contributors
  * Fixed typos in Description
* Changes to `spatial_data()`
  * Used `spatstat.core::as.solist()` to specify appropriate class to lists
* Changes to `spatial_power()`
  * Fixed typos in documentation
  * Moved `cascon` argument from `spatial_power()` to `spatial_plots()`
  * Estimates case-only power (lower tail) and case/control (lower and upper tail) and captures both outputs
  * Used `spatstat.core::as.solist()` to specify appropriate class to lists
* Changes to `spatial_plots()`:
  * Added `cascon` argument to toggle the display of case-only power (lower tail) or case/control (lower and upper tail) output
  * Added `plot_title` argument to toggle the display of titles in plots. Changed the default title names. 
  * Removed main panel title in plots
  * Fixed default colors and removed mislabled 'midpoint' color
  * Removed annotation from `plot.ppp()` in plot #3 and plot #2 (if points are not plotted)
  * Added `scale` argument to scale text for higher resolution plots
  * Added `horizontal` argument to toggle the display of the color key to be on the right (vertical) or bottom (horizontal) of plots
  * Added `plot_axes` argument to toggle the display of axes in plots
  * Added `plot_square` argument to toggle the margins of plots
  * Changed the value of `cex.axis` and `cex` in plots
* Changes to `jitter_power()`
  * Specify all packages for functions
  * Moved `cascon` argument from `jitter_power()` to `spatial_plots()`
  * Estimates case-only power (lower tail) and case/control (lower and upper tail) and captures both outputs
  * Used `spatstat.core::as.solist()` to specify appropriate class to lists
* Changes to vignette
  * Set global chunk options
  * Named code chunks
  * Increased the `sim_total` value for a more realistic example
  * Specify all packages for functions
  * Updated code to reflect updates to functions in `sparrpowR`
* Added utils.R file
  * Moved an internal helper functions to a utils.R file
  * Removed this internal helper functions from `jitter_power()`, `spatial_power()`, and `spatial_plots()`
* Added zzz.R file
* Added package.R file
