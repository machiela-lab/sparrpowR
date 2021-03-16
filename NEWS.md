# sparrpowR (development version)

# sparrpowR 0.2.1
* Updated vignette with appropriate hyperlinks

# sparrpowR 0.2.0
* Updates to dependencies
  * Updated `spatstat` package to new subsetted packages based on feedback from the Spatstat Team (Adrian Baddeley and Ege Rubak). `spatstat.geom` and `spatstat.core` packages replace `spatstat` package in Depends
  * Imports `lifecycle` package to document deprecated argument `cascon` in `spatial_power()` and `jitter_power()` functions
  * Replaced `parallel` and `doParallel` packages in Imports with `doFuture`, `doRNG`, and `future` packages to allow for parallel processing in `spatial_power()` and `jitter_power()` functions to work across all CRAN environments
  * Removed `utils` package from Imports because the progress bar in `spatial_power()` and `jitter_power()` functions is now produced with a helper function in utils.R that imports the `iterators` package
  * Removed `tidyverse` package from Suggests
* Changes to DESCRIPTION
  * Added ORCID for all authors and contributors
  * Fixed typos in Description
* Changes to `spatial_data()`
  * Used `as.solist()` to specify appropriate class to lists
* Changes to `spatial_power()`
  * Added `p_correct` argument to apply a multiple testing correction
  * Fixed typos in documentation
  * Deprecated `cascon` argument (moved to `spatial_plots()` function)
  * Estimates case-only power (lower tail) and case/control (lower and upper tail) and captures both outputs. Case-only is now a one-tailed hypothesis test by default. The arguments `lower_tail` and `upper_tail` have been replaced with argument `alpha`
  * Used `as.solist()` to specify appropriate class to lists
* Changes to `spatial_plots()`:
  * Added `cascon` argument to toggle the display of case-only power (lower tail) or case/control (lower and upper tail) output
  * Added `plot_title` argument to toggle the display of titles in plots. Changed the default title names.
  * Removed main panel title in plots
  * Fixed default colors and removed mislabled `midpoint` color
  * Removed annotation from `plot.ppp()` in plot #3 and plot #2 (if points are not plotted)
  * Added `scale` argument to scale text for higher resolution plots
  * Added `horizontal` argument to toggle the display of the color key to be on the right (vertical) or bottom (horizontal) of plots
  * Added `plot_axes` argument to toggle the display of axes in plots
  * Added `plot_square` argument to toggle the margins of plots
  * Changed the value of `cex.axis` and `cex` in plots
  * Updated documentation with correct order of `cols`
* Changes to `jitter_power()`
  * Added `p_correct` argument to apply a multiple testing correction
  * Specified all packages for functions
  * Deprecated `cascon` argument (moved to `spatial_plots()` function)
  * Estimates case-only power (lower tail) and case/control (lower and upper tail) and captures both outputs. Case-only is now a one-tailed hypothesis test by default. The arguments `lower_tail` and `upper_tail` have been replaced with argument `alpha`
  * Used `as.solist()` to specify appropriate class to lists
  * Updated example to reflect new updates
* Changes to vignette
  * Set global chunk options
  * Named code chunks
  * Increased the `sim_total` value for a more realistic example
  * Specify all packages for functions
  * Updated code to reflect updates to functions in `sparrpowR`
  * Updated code to use `sf` package
  * Updated explanation of the `cascon` argument to plot various hypothesis tests
  * Changed `cols` values to be more visible 
  * Used `cut` instead of `reclassify` in `raster` package for categorical plot
  * Created a new section called "Advanced Features" that has a new example run in parallel with a False Discovery Rate procedure. 
  * Added detail about fewer-than-expected simulated point locations in the first example
  * Updated plots that spatially project data with CRS strings for PROJ6
* Added utils.R file
  * Removed the helper `comb()` function functions from `jitter_power()`, `spatial_power()`, and `spatial_plots()` and added to the utils.R file
  * Removed `utils` package from Imports because the progress bar in `spatial_power()` and `jitter_power()` functions is now produced with a helper `progBar()` function
* Added zzz.R file
* Added package.R file
* Updated testthat checks for parallelization and multiple testing correction in `spatial_power()` and `jitter_power()` functions