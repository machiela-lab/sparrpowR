#' The sparrpowR Package: Power Analysis to Detect Spatial Relative Risk Clusters
#'
#' Computes the statistical power for the spatial relative risk function.
#'
#' @details For a two-group comparison (e.g., cases v. controls) the 'sparrpowR' package calculates the statistical power to detect clusters using the kernel-based spatial relative risk function that is estimated using the 'sparr' package. Details about the 'sparr' package methods can be found in the tutorial: Davies et al. (2018) \doi{10.1002/sim.7577}.  Details about kernel density estimation can be found in J. F. Bithell (1990) \doi{10.1002/sim.4780090616}.  More information about relative risk functions using kernel density estimation can be found in J. F. Bithell (1991) \doi{10.1002/sim.4780101112}.
#' 
#' This package provides a function to compute the statistical power for the spatial relative risk function with various theoretical spatial sampling strategies. The 'sparrpowR' package also provides a function to compute the statistical power for the spatial relative risk function for scenarios where one group (e.g., cases) have been observed and a theoretical sampling strategy for the second group (e.g., controls) is desired. The 'sparrpowR' package also provides visualization of data and statistical power.
#' 
#' Key content of the 'sparrpowR' package include:\cr
#' 
#' \bold{Theoretical Spatial Sampling}
#' 
#' \code{\link{spatial_data}} Generates random two-group data for a spatial relative risk function.
#' 
#' \bold{Statistical Power}
#' 
#' \code{\link{spatial_power}} Computes the statistical power of a spatial relative risk function using randomly generated data.
#' 
#' \code{\link{jitter_power}} Computes the statistical power of a spatial relative risk function using previously collected data.
#' 
#' \bold{Data Visualization}
#' 
#' \code{\link{spatial_plots}} Visualizes multiple plots of output from \code{\link{spatial_data}}, \code{\link{spatial_power}} and \code{\link{jitter_power}} functions.
#' 
#' @name sparrpowR-package
#' @aliases sparrpowR-package sparrpowR
#' @docType package
#' 
#' @section Dependencies: The 'sparrpowR' package relies heavily upon \code{\link{sparr}}, \code{\link{spatstat.random}}, \code{\link{spatstat.geom}}, and \code{\link{raster}} for computing the statistical power and visualizing the output. Computation can be performed in parallel using \code{\link{doFuture}}, \code{\link[future]{multisession}}, \code{\link{doRNG}}, and \code{\link[foreach]{foreach}}. Basic visualizations rely on the \code{\link[spatstat.geom]{plot.ppp}} and \code{\link[fields]{image.plot}} functions.
#' 
#' @author Ian D. Buller\cr \emph{Occupational and Environmental Epidemiology Branch, Division of Cancer Epidemiology and Genetics, National Cancer Institute, National Institutes of Health, Rockville, Maryland, USA.} \cr\cr
#' Derek W. Brown\cr \emph{Integrative Tumor Epidemiology Branch, Division of Cancer Epidemiology and Genetics, National Cancer Institute, National Institutes of Health, Rockville, Maryland, USA.}
#' 
#' Maintainer: I.D.B. \email{ian.buller@@nih.gov}
#'
#' @keywords package
NULL

#' @importFrom doFuture registerDoFuture
#' @importFrom doRNG %dorng%
#' @importFrom fields image.plot
#' @importFrom foreach %do% %dopar% foreach setDoPar
#' @importFrom future multisession plan 
#' @importFrom graphics text par
#' @importFrom grDevices colorRampPalette
#' @importFrom iterators icount
#' @importFrom lifecycle badge deprecate_warn deprecated is_present
#' @importFrom raster raster values reclassify
#' @importFrom sp coordinates gridded
#' @importFrom sparr risk
#' @importFrom stats na.omit rnorm sd
#' @importFrom spatstat.geom as.solist disc marks plot.anylist plot.ppp ppp rsyst shift superimpose unit.square
#' @importFrom spatstat.random rNeymanScott rpoispp runifdisc runifpoint
NULL
