#' Plots for statistical power estimates of the SRR function
#' 
#' Create multiple plots of output from \code{\link{spatial_data}}, \code{\link{spatial_power}} and \code{\link{jitter_power}} functions.
#' 
#' @param input An object of class "ppplist" from the \code{\link{spatial_data}} function or an object of class "list" from the \code{\link{spatial_power}} or \code{\link{jitter_power}} functions.
#' @param p_thresh A numeric value between 0 and 1 (default = 0.8) for the power threshold.
#' @param cascon Logical. If TRUE, displays the statistical power to detect case clusters and control clusters. If FALSE (the default), displays the statistical power to detect case clusters only. 
#' @param n_sim Integer. The number of simulated iterations to plot. The default is one (1).
#' @param cols Character string of length four (4) specifying the colors for plotting: 1) sufficiently powered, 2) insufficiently powered, 3) case locations, 4) control locations. The default colors in hex are \code{c("#000000", "#cccccc", "#ff0000", "#0000ff")} or \code{c("grey0", "grey80", "red", "blue")}.
#' @param chars Vector of integers or character string of length two (2) for symbols of case and control locations. Default is \code{c(1,1)}. 
#' @param sizes Vector of integers of length two (2) for the size of the symbols for case and control locations. Default is \code{c(1,1)}. 
#' @param scale Integer. A graphical expansion factor (default is 1) for text (and point) size within plots. Intended for scaling plot features with plot resolution.
#' @param plot_pts Logical. If TRUE (the default), the points from the first simulation iteration will be added to second plot. Not if FALSE.
#' @param plot_title Logical. If TRUE (the default), a title will be included in the plot(s). Not if FALSE.
#' @param plot_text Logical. If TRUE, the local statistical power will be printed at each grid cell. Not if FALSE (the default).
#' @param plot_axes Logical. If TRUE, the axes with labels will be included in the plot(s). Not if FALSE (the default).
#' @param ... Arguments passed to \code{\link[spatstat.core]{plot.ppp}} and \code{\link[fields]{image.plot}} for additional graphical features.
#'
#' @return This function produces up to three plots: 1) example input, 2) local power, and 3) local power above a threshold. If the input is from the \code{\link{spatial_data}} function, this function will only display the first plot. 
#' 
#' @importFrom fields image.plot
#' @importFrom graphics text
#' @importFrom grDevices colorRampPalette
#' @importFrom raster raster values reclassify
#' @importFrom sp coordinates gridded
#' @importFrom spatstat.core plot.ppp plot.anylist
#' @importFrom stats na.omit
#' @export 
#'
#' @examples
#' \donttest{
#' spatial_plots(input = sim_power)
#' }
#' 
spatial_plots <- function(input,
                          p_thresh = 0.8,
                          cascon = FALSE,
                          n_sim = 1,
                          cols = c("#000000", "#cccccc", "#ff0000", "#0000ff"),
                          chars = c(1,1),
                          sizes = c(1,1),
                          scale = 1, 
                          plot_pts = TRUE,
                          plot_title = TRUE,
                          plot_text = FALSE,
                          plot_axes = FALSE,
                          ...) {
  
  # Case Clusters Only or Case and Control Clusters
  if(cascon == TRUE) {
    pvalprop <- data.frame("x" = input$rx, "y" = input$ry, "z" = input$pval_prop_cascon)
  } else {
    pvalprop <- data.frame("x" = input$rx, "y" = input$ry, "z" = input$pval_prop_cas)
  }
  
  # Default Titles for plots
  if(plot_title == TRUE) {
    plot3_title <- paste("Local power:\nProportion significant above",
                         p_thresh,
                         "threshold",
                         sep = " ")
    plot_titles <- c("First iteration(s)\nof simulated data",
                     "Local power:\nProportion of simulations significant",
                     plot3_title)
  } else {
    plot_titles <- c("", "", "")
  }
  
  # Scale the title size
  par(cex.main = 1 * scale)
  
  # If input from spatial_data() function
  if("ppplist" %in% class(input)) {
    return(spatstat.core::plot.anylist(input[1:n_sim], 
                                       pch = chars,
                                       cex = sizes * scale,
                                       cols = c(cols[3], cols[4]),
                                       leg.side = "bottom",
                                       leg.args = list(cex.axis = 0.9 * scale,
                                                       cex = 1 * scale,
                                                       pch = chars),
                                       main = plot_titles[1],
                                       main.panel = "",
                                       ...))
  }
  
  # Plot 1: Example input
  p1 <- spatstat.core::plot.ppp(input$sim, 
                                pch = chars, 
                                cex = sizes * scale,
                                cols = c(cols[3], cols[4]),
                                leg.side = "bottom",
                                leg.args = list(cex.axis = 0.9 * scale,
                                                cex = 1 * scale,
                                                pch = chars),
                                main = plot_titles[1],
                                main.panel = "",
                                ...)
  if (plot_axes == TRUE) {
    graphics::axis(1)
    graphics::axis(2)
  }
  
  # Plot 2: Power, Continuous
  ## Create proportion significant raster
  lrr_narm <- stats::na.omit(pvalprop) # remove NAs
  sp::coordinates(lrr_narm) <- ~ x + y # coordinates
  sp::gridded(lrr_narm) <- TRUE # gridded
  pvalprop_raster <- raster::raster(lrr_narm)
  pvalprop <- NULL # conserve memory
  lrr_narm <- NULL # conserve memory
  
  ## Colors for raster
  rampcols <- grDevices::colorRampPalette(colors = c(cols[1], cols[2]),
                                          space = "Lab")(length(raster::values(pvalprop_raster)))
  rampbreaks <- seq(0, 1, 
                    length.out = length(raster::values(pvalprop_raster)) + 1)
  ## Continuous Output
  p2 <- spatstat.core::plot.ppp(input$sim, 
                                pch = chars, 
                                cex = sizes * scale,
                                cols = c(cols[3], cols[4]),
                                leg.side = "bottom",
                                leg.args = list(cex.axis = 0.9 * scale,
                                                cex = 1 * scale,
                                                pch = chars),
                                main = plot_titles[2],
                                ...)
  fields::image.plot(pvalprop_raster, 
                     col = rampcols,
                     breaks = rampbreaks,
                     axes = FALSE,
                     cex.lab = 1 * scale,
                     xlab = "",
                     ylab = "",
                     cex = 1 * scale,
                     axis.args = list(cex.axis = 0.67 * scale,
                                      las = 0),
                     add = TRUE,
                     #horizontal = TRUE,
                     legend.args = list(text = "Power",
                                        side = 3,
                                        cex = 0.67 * scale),
                     ...)
  if (plot_pts == TRUE) {
    spatstat.core::plot.ppp(input$sim, 
                            pch = chars, 
                            cex = sizes * scale,
                            cols = c(cols[3], cols[4]),
                            add = TRUE,
                            leg.args = list(cex.axis = 0.9 * scale,
                                            cex = 1 * scale,
                                            pch = chars),
                            ...)
  }
  if (plot_axes == TRUE) {
    graphics::axis(1)
    graphics::axis(2)
  }
  
  rampbreaks <- NULL # conserve memory
  rampcols <- NULL # convserve memory
  
  # Plot 2: Power, labled
  ## Reclassify raster of proportion significant
  #### Here: power = 80
  pvalprop_reclass <- raster::reclassify(pvalprop_raster,
                                         c(-Inf, p_thresh-0.0000001, 1,
                                           p_thresh-0.0000001, Inf, 2))
  pvalprop_raster <- NULL # conserve memory
  
  ## Categorical Output
  p3 <- spatstat.core::plot.ppp(input$sim, 
                                pch = chars, 
                                cex = sizes * scale,
                                cols = c("transparent", "transparent"),
                                leg.side = "bottom",
                                leg.args = list(annotate = FALSE),
                                main = plot_titles[3],
                                ...)
  fields::image.plot(pvalprop_reclass, 
                     add = TRUE,
                     col = cols[1:2],
                     breaks = c(1, 1.5, 2),
                     axes = FALSE,
                     cex.lab = 1 * scale,
                     xlab = "",
                     ylab = "",
                     cex = 1 * scale,
                     #horizontal = TRUE,
                     axis.args = list(cex.axis = 0.67 * scale,
                                      labels = c("insufficient", "sufficient"),
                                      las = 0,
                                      at = c(1.25, 1.75)),
                     legend.args = list(text = "Power",
                                        side = 3,
                                        cex = 0.67 * scale),
                     ...)
  ### Add text of local power to each knot
  if(plot_text == TRUE) {
    graphics::text(x = input$rx, y = input$ry, input$pval_prop, col = cols[3], cex = 0.5)
  }
  if (plot_axes == TRUE) {
    graphics::axis(1)
    graphics::axis(2)
  }
}
