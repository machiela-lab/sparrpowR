#' Plots for statistical power estimates of the SRR function
#' 
#' Create multiple plots of output from \code{\link{spatial_data}}, \code{\link{spatial_power}} and \code{\link{jitter_power}} functions.
#' 
#' @param input An object of class "ppplist" from the \code{\link{spatial_data}} function or an object of class "list" from the \code{\link{spatial_power}} or \code{\link{jitter_power}} functions.
#' @param p_thresh A numeric value between 0 and 1 (default = 0.8) for the power threshold.
#' @param cascon Logical. If TRUE, displays the statistical power to detect case clusters and control clusters (two-tailed hypothesis). If FALSE (the default), displays the statistical power to detect case clusters only (one-tailed, lower-tail hypothesis). 
#' @param n_sim Integer. The number of simulated iterations to plot. The default is one (1).
#' @param cols Character string of length four (4) specifying the colors for plotting: 1) insufficiently powered, 2) sufficiently powered, 3) case locations, 4) control locations. The default colors in hex are \code{c("#000000", "#CCCCCC", "#FF0000", "#0000FF")} or \code{c("grey0", "grey80", "red", "blue")}.
#' @param chars Vector of integers or character string of length two (2) for symbols of case and control locations. Default is \code{c(1,1)}. 
#' @param sizes Vector of integers of length two (2) for the size of the symbols for case and control locations. Default is \code{c(1,1)}. 
#' @param scale Integer. A graphical expansion factor (default is 1) for text (and point) size within plots. Intended for scaling plot features with plot resolution.
#' @param plot_pts Logical. If TRUE (the default), the points from the first simulation iteration will be added to second plot. Not if FALSE.
#' @param plot_title Logical. If TRUE (the default), a title will be included in the plot(s). Not if FALSE.
#' @param plot_text Logical. If TRUE, the local statistical power will be printed at each grid cell. Not if FALSE (the default).
#' @param plot_axes Logical. If TRUE, the axes with labels will be included in the plot(s). Not if FALSE (the default).
#' @param plot_square Logical. If TRUE, the plot will have margins with similar units. Not if FALSE (the default).
#' @param horizontal Logical. If TRUE (the default), the color key will be displayed horizontally, below the plots. If FALSE, the color key will be displayed vertically, to the right of the plots.
#' @param ... Arguments passed to \code{\link[spatstat.geom]{plot.ppp}} and \code{\link[fields]{image.plot}} for additional graphical features.
#'
#' @return This function produces up to three plots: 1) example input, 2) local power, and 3) local power above a threshold if the input is from the \code{\link{spatial_power}} r \code{\link{jitter_power}} functions. If the input is from the \code{\link{spatial_data}} function, this function will only display the first plot. 
#' 
#' @importFrom fields image.plot
#' @importFrom graphics text par
#' @importFrom grDevices colorRampPalette
#' @importFrom raster cut raster values
#' @importFrom sp coordinates gridded
#' @importFrom spatstat.geom plot.anylist plot.ppp 
#' @importFrom stats na.omit
#' @export 
#'
#' @examples
#' \donttest{
#'  # run spatial_power(), jitter_power(), or spatial_data()
#'  sim_power <- spatial_power(x_case = c(0.25, 0.5, 0.75),
#'                             y_case = c(0.75, 0.25, 0.75),
#'                             samp_case = "MVN", 
#'                             samp_control = "MVN",
#'                             x_control = c(0.25, 0.5, 0.75),
#'                             y_control = c(0.75, 0.25, 0.75),
#'                             n_case = 100,
#'                             n_control = c(100,500,300),
#'                             s_case = c(0.05,0.01,0.05),
#'                             s_control = 0.05,
#'                             verbose = FALSE)
#'                
#'  # run spatial_plots()
#'  spatial_plots(input = sim_power)
#' }
#' 
spatial_plots <- function(input,
                          p_thresh = 0.8,
                          cascon = FALSE,
                          n_sim = 1,
                          cols = c("#000000", "#CCCCCC", "#FF0000", "#0000FF"),
                          chars = c(1,1),
                          sizes = c(1,1),
                          scale = 1, 
                          plot_pts = TRUE,
                          plot_title = TRUE,
                          plot_text = FALSE,
                          plot_axes = FALSE,
                          plot_square = FALSE,
                          horizontal = TRUE,
                          ...) {
  
  # Graphical parameters
  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op))
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
  
  if (plot_square == TRUE) { square <- "s"} else { square <- "m" }
  
  # Scale the title size
  graphics::par(cex.main = 1 * scale, pty = square)
  
  # If input from spatial_data() function
  if("ppplist" %in% class(input)) {
    return(spatstat.geom::plot.anylist(input[1:n_sim], 
                                       pch = chars,
                                       cex = sizes * scale,
                                       cols = c(cols[3], cols[4]),
                                       leg.side = "bottom",
                                       leg.args = list(cex.axis = 0.75 * scale,
                                                       cex = 2 * scale,
                                                       pch = chars),
                                       main = plot_titles[1],
                                       main.panel = "",
                                       ...))
  }
  
  # Plot 1: Example input
  p1 <- spatstat.geom::plot.ppp(input$sim, 
                                pch = chars, 
                                cex = sizes * scale,
                                cols = c(cols[3], cols[4]),
                                leg.side = "bottom",
                                leg.args = list(cex.axis = 0.75 * scale,
                                                cex = 2 * scale,
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
  if (plot_pts == TRUE) {
    p2 <- spatstat.geom::plot.ppp(input$sim, 
                                  pch = chars, 
                                  cex = 0,
                                  cols = c(cols[3], cols[4]),
                                  leg.side = "bottom",
                                  leg.args = list(cex.axis = 0.75 * scale,
                                                  cex = 2 * scale,
                                                  pch = chars),
                                  main = plot_titles[2],
                                  ...)
  } else {
    p2 <- spatstat.geom::plot.ppp(input$sim, 
                                  pch = chars, 
                                  cex = sizes * scale,
                                  cols = c("transparent", "transparent"),
                                  leg.side = "bottom",
                                  leg.args = list(annotate = FALSE),
                                  main = plot_titles[2],
                                  ...)
  }
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
                     horizontal = horizontal,
                     legend.args = list(text = "Power",
                                        side = 3,
                                        cex = 0.67 * scale),
                     ...)
  if (plot_pts == TRUE) {
    spatstat.geom::plot.ppp(input$sim, 
                            pch = chars, 
                            cex = sizes * scale,
                            cols = c(cols[3], cols[4]),
                            add = TRUE,
                            leg.args = list(cex.axis = 0.75 * scale,
                                            cex = 2 * scale,
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
  pvalprop_reclass <- raster::cut(pvalprop_raster,
                                  breaks = c(-Inf, p_thresh, Inf))
  pvalprop_raster <- NULL # conserve memory
  
  ## Categorical Output
  p3 <- spatstat.geom::plot.ppp(input$sim, 
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
                     horizontal = horizontal,
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
