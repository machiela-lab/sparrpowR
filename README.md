# sparrpowR

### Power Analysis to Detect Spatial Relative Risk Clusters 

### Simulation study and environmental cancer epidemiologic case study 

Created by: Derek Brown ([@derekbrown12](https://github.com/derekbrown12)) and Ian Buller ([@idblr](https://github.com/idblr))

Created on: April, 2020

Objective: Calculate local power of kernel-based spatial relative risk estimation based on the [sparr](https://cran.r-project.org/web/packages/sparr/index.html) package in *R*


# sparrpowR

## Power Analysis to Detect Spatial Relative Risk Clusters in R

A suite of `R` functions to calculate the statistical power to detect clusters using kernel-based spatial relative risk functions that are estimated using the  [sparr](https://cran.r-project.org/web/packages/sparr/index.html) package.

## Getting Started

The R workflow was built with `R version 3.6.3 (2020-02-29) -- "Holding the Windsock"` and requires several established `R` packages. 

```
  loadedPackages <- c("dplyr", "fields", "graphics", "maptools", "pgirmess", "raster", "rgeos", "sp", "sparr", "spatstat", "tibble")
  invisible(lapply(loadedPackages, require, character.only = T))
```

To handle mass cytometry data, you will likely need the `BiocManager` package suite.

### Prerequisites

Working with spatial data (e.g., `spatialpointsdataframe`, `raster` and `polygons`) may require you to have the most recent version of [GDAL](https://trac.osgeo.org/gdal/wiki/BuildingOnMac). 

### Installing

To run the gateRR() function you will need to source:
1. [lotrrs.R](https://github.com/idblr/gateRR/blob/master/code/R_code/R_functions/lotrrs.R)
2. [lrr_plot.R](https://github.com/idblr/gateRR/blob/master/code/R_code/R_functions/lrr_plot.R)
3. [pval_plot.R](https://github.com/idblr/gateRR/blob/master/code/R_code/R_functions/pval_plot.R)
4. [pval_correct.R](https://github.com/idblr/gateRR/blob/master/code/R_code/R_functions/pval_correct.R)
5. [asy.cauchy.fix.R](https://github.com/idblr/gateRR/blob/master/code/R_code/R_functions/asy.cauchy.fix.R)

The vignettes, [sparr_ratio.Rmd](https://github.com/idblr/gateRR/blob/master/code/Rmd_code/sparr_ratio.Rmd) or [gateRR.Rmd](https://github.com/idblr/gateRR/blob/master/code/Rmd_code/gateRR.Rmd), start with how to create toy data to demonstrate the tools 

Stay-tuned for a real-world mass cytometry dataset. 

## Running the tests

For an example workflow of data simulation and power calculation see the 

created with the [vignette.Rmd](https://github.com/idblr/sparrpowR/blob/master/code/vignette.Rmd)

For separate examples of the `spatial_data()` and `spatial_power()` functions see the 

the workhorse R function `lotrrs()` see the vignette in [sparr_ratio.Rmd](https://github.com/idblr/gateRR/blob/master/code/Rmd_code/sparr_ratio.Rmd)

To see an example workflow of a gating strategy using `gateRR()` see the vignette in [gateRR.Rmd](https://github.com/idblr/gateRR/blob/master/code/Rmd_code/gateRR.Rmd)

## In Development

Adaptation of this R package to a webtool by the National Cancer Institute Center for Biomedical Informatics and Information Technology

## Authors

* **Ian Buller** - *Initial heavycoding with expertise in spatial statistics * - [GitHub](https://github.com/idblr)
* **Derek Brown** - *Code QA/QC with expertise in power calculations* - [GitHub](https://github.com/derekbrown12)
* **Tim Myers** - *Adaptation to an R package* - [GitHub](https://github.com/timyers)

See also the list of [contributors](https://github.com/idblr/sparrpowR/graphs/contributors) who participated in this project.

## License

This project is licensed under the Apache 2.0 License - see the [LICENSE.md](https://github.com/idblr/sparrpowR/blob/master/LICENSE) file for details

## Acknowledgments

* Huge s/o to Tilman Davies [GitHub](https://github.com/tilmandavies) and Martin Hazelton [link](https://www.stats.otago.ac.nz/?people=martin_hazelton) for the `sparr` [R package](https://github.com/cran/sparr). Makes this all feasible.
* Thank you to the National Cancer Institute Division of Cancer Epidemiology and Genetics for funding through the Informatics Tool Challenge Award 2020