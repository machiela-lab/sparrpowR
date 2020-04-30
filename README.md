# sparrpowR

## Power Analysis to Detect Spatial Relative Risk Clusters in R

A suite of `R` functions to calculate the statistical power to detect clusters using kernel-based spatial relative risk functions that are estimated using the  [sparr](https://cran.r-project.org/web/packages/sparr/index.html) package.

## Getting Started

The R workflow was built with `R version 3.6.3 (2020-02-29) -- "Holding the Windsock"` and requires several established `R` packages. 

```
  loadedPackages <- c("doParallel", "fields", "foreach", "parallel", "sp", "sparr", "spatstat", "raster")
  invisible(lapply(loadedPackages, require, character.only = T))
```

### Prerequisites

Working with spatial data (e.g., `SpatialPointsDataFrame`, `raster`, etc) may require you to have the most recent version of [GDAL](https://trac.osgeo.org/gdal/wiki/BuildingOnMac). 

### Installing

To run the workflow you will need to source:

1. [spatial_data.R](https://github.com/idblr/sparrpowR/blob/master/code/R_functions/spatial_data.R)
2. [spatial_power.R](https://github.com/idblr/sparrpowR/blob/master/code/R_functions/spatial_power.R)
3. [spatial_plots.R](https://github.com/idblr/sparrpowR/blob/master/code/R_functions/spatial_plots.R)
4. [jitter_power.R](https://github.com/idblr/sparrpowR/blob/master/code/R_functions/jitter_power.R)

The vignette, [vignette.Rmd](https://github.com/idblr/sparrpowR/blob/master/code/Rmd/vignette.Rmd), starts with how to simulate case/control point-level data and calculate the statistical power for a theoretical example of a spatial relative risk function in Washington, D.C.

## Running the tests

For separate examples of the `spatial_data()` and `spatial_power()` functions see [example_sparrpowR.R](https://github.com/idblr/sparrpowR/blob/master/code/example_sparrpowR.R).

For an examples of the `spatial_data()` and `jitter_power()` functions see [example_jitter.R](https://github.com/idblr/sparrpowR/blob/master/code/example_jitter.R).

## In Development

Adaptation of this R package to a webtool by the National Cancer Institute Center for Biomedical Informatics and Information Technology [website](https://datascience.cancer.gov/).

## Authors

* **Ian Buller** - *Initial heavycoding with expertise in spatial statistics * - [GitHub](https://github.com/idblr)
* **Derek Brown** - *Code conceptualization and QA/QC with expertise in power calculations* - [GitHub](https://github.com/derekbrown12)
* **Tim Myers** - *Adaptation to an R package* - [GitHub](https://github.com/timyers)

See also the list of [contributors](https://github.com/idblr/sparrpowR/graphs/contributors) who participated in this project.

## License

This project is licensed under the Apache 2.0 License - see the [LICENSE.md](https://github.com/idblr/sparrpowR/blob/master/LICENSE) file for details

## Acknowledgments

* Huge s/o to Tilman Davies [GitHub](https://github.com/tilmandavies) and Martin Hazelton [link](https://www.stats.otago.ac.nz/?people=martin_hazelton) for the `sparr` [R package](https://github.com/cran/sparr). Makes this all feasible.