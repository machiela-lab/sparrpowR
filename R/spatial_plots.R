#' Plots for statistical power estimates of the SRR function
#' 
#' Create multiple plots of output from \code{\link{spatial_data}}, \code{\link{spatial_power}} and \code{\link{jitter_power}} functions.
#' 
#' @param input An object of class "ppplist" from the \code{\link{spatial_data}} function or an object of class "list" from the \code{\link{spatial_power}} or \code{\link{jitter_power}} functions.
#' @param p_thresh A numeric value between 0 and 1 (default = 0.8) for the power threshold.
#' @param n_sim Integer. The number of simulated iterations to plot. The default is one (1).
#' @param cols Character string of length five (5) specifying the colors for plotting: 1) sufficiently powered, midpoint, insufficiently powered, case locations, control locations. The default colors in hex are \code{c("#0000ff", "#00ff00", "#ff0000", "#a020f0", "#ffa500")} or \code{c("grey0", "grey80", "grey100", "red", "blue")}.
#' @param chars Vector of integers or character string of length two (2) for symbols of case and control locations. Default is \code{c(1,1)}. 
#' @param sizes Vector of integers of length two (2) for the size of the symbols for case and control locations. Default is \code{c(1,1)}. 
#' @param plot_pts Logical. If TRUE (the default), the points from the first simulation iteration will be added to second plot. Not if FALSE.
#' @param plot_text Logical. If TRUE, the local statistical power will be printed at each grid cell. Not if FALSE (the default).
#' @param ... Arguments passed to \code{\link[spatstat]{plot.ppp}} and \code{\link[fields]{image.plot}} for additional graphical features.
#'
#' @return This function produces up to three plots: 1) example input, 2) local power, and 3) local power above a threshold. If the input is from the \code{\link{spatial_data}} function, this function will only display the first plot. 
#' 
#' @importFrom stats na.omit
#' @importFrom graphics text
#' @importFrom sp coordinates gridded
#' @importFrom spatstat plot.ppp plot.anylist
#' @importFrom raster raster values reclassify
#' @importFrom grDevices colorRampPalette
#' @importFrom fields image.plot
#' 
#' @export 
#'
#' @examples
#' \donttest{
#' spatial_plots(input = sim_power)
#' }
#' 
spatial_plots <- function(input,
                          p_thresh = 0.8,
                          n_sim = 1,
                          cols = c("#000000", "#cccccc", "#ffffff", "#ff0000", "#0000ff"),
                          chars = c(1,1),
                          sizes = c(1,1),
                          plot_pts = TRUE,
                          plot_text = FALSE,
                          ...) {
  
  
  if("ppplist" %in% class(input)){
    return(spatstat::plot.anylist(input[1:n_sim], 
                    pch = chars,
                    cex = sizes,
                    cols = c(cols[4], cols[5]),
                    leg.side = "bottom",
                    leg.args = list(cex.axis = 0.9, cex = 1, pch = chars),
                    main = "First iteration(s)\nof simulated data",
                    ...
    )
    )
  }
  
  # Plot 1: Example input
  p1 <- spatstat::plot.ppp(input$sim, 
                           pch = chars, 
                           cex = sizes,
                           cols = c(cols[4], cols[5]),
                           leg.side = "bottom",
                           leg.args = list(cex.axis = 0.9, cex = 1, pch = chars),
                           main = "First iterations of simulated data",
                           ...)
  
  # Plot 2: Power, Continuous
  ## Create proportion significant raster
  pvalprop <- data.frame("x" = input$rx, "y" = input$ry, "z" = input$pval_prop)
  lrr_narm <- stats::na.omit(pvalprop) # remove NAs
  sp::coordinates(lrr_narm) <- ~ x + y # coordinates
  sp::gridded(lrr_narm) <- TRUE # gridded
  pvalprop_raster <- raster::raster(lrr_narm)
  pvalprop <- NULL # conserve memory
  lrr_narm <- NULL # conserve memory
  
  ## Colors for raster
  rampcols <- grDevices::colorRampPalette(colors = c(cols[1], cols[2]),
                                          space="Lab"
  )(length(raster::values(pvalprop_raster)))
  rampbreaks <- seq(0, 1, 
                    length.out = length(raster::values(pvalprop_raster))+1
  )
  ## Continuous Output
  p2 <- spatstat::plot.ppp(input$sim, 
                           pch = chars, 
                           cex = sizes,
                           cols = c(cols[4], cols[5]),
                           leg.side = "bottom",
                           leg.args = list(cex.axis = 0.9, cex = 1, pch = chars),
                           main = "Local power:\nProportion of simulations significant",
                           ...)
  fields::image.plot(pvalprop_raster, 
                     col = rampcols,
                     breaks = rampbreaks,
                     axes = FALSE,
                     cex.lab = 1,
                     xlab = "",
                     ylab = "",
                     cex = 1,
                     bigplot = c(0.25, 0.8, 0.2, 0.8),
                     smallplot = c(0.82, 0.84, 0.32, 0.8),
                     axis.args = list(cex.axis = 0.67),
                     add = TRUE,
                     legend.args = list(text = "Power",
                                        side = 4,
                                        line = 2,
                                        cex = 0.67
                     ),
                     ...)
  if(plot_pts == TRUE) {
    spatstat::plot.ppp(input$sim, 
                       pch = chars, 
                       cex = sizes,
                       cols = c(cols[4], cols[5]),
                       add = TRUE,
                       ...)
  }
  
  rampbreaks <- NULL # conserve memory
  rampcols <- NULL # convserve memory
  
  # Plot 2: Power, labled
  ## Reclassify raster of proportion significant
  #### Here: power = 80
  pvalprop_reclass <- raster::reclassify(pvalprop_raster,
                                         c(-Inf, p_thresh-0.0000001, 1,
                                           p_thresh-0.0000001, Inf, 2
                                         )
  )
  pvalprop_raster <- NULL # conserve memory
  
  ## Categorical Output
  p3 <- spatstat::plot.ppp(input$sim, 
                           pch = chars, 
                           # cex = sizes,
                           cols = c("transparent", "transparent"),
                           leg.side = "bottom",
                           leg.args = list(cex.axis = 0.0000000000000001),
                           main = paste("Local power:\nProportion significant above",
                                        p_thresh,
                                        "threshold",
                                        sep = " "),
                           ...)
  fields::image.plot(pvalprop_reclass, 
                     add = TRUE,
                     col = cols[1:2],
                     breaks = c(1, 1.5, 2),
                     axes = FALSE,
                     cex.lab = 1,
                     xlab = "",
                     ylab = "",
                     cex = 1,
                     bigplot = c(0.25, 0.8, 0.2, 0.8),
                     smallplot = c(0.82, 0.84, 0.32, 0.8),
                     axis.args = list(cex.axis = 0.67,
                                      labels = c("insufficient", "sufficient"),
                                      at = c(1.25, 1.75)
                     ),
                     legend.args = list(text = "Power",
                                        side = 4,
                                        line = 2,
                                        cex = 0.67
                     ),
                     ...)
  ### Add text of local power to each knot
  if(plot_text == TRUE){
    graphics::text(x = input$rx, y = input$ry, input$pval_prop, col = cols[3], cex = 0.5)
  }
  pvalprop_reclass <- NULL # conserve memory
}