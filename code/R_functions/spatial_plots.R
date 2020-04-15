# ------------------------------------------ #
# Function to Estimate the Power of a Spatial Relative Risk using Simulated Data
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: April 13, 2020
#
# Recently modified by: @idblr
# Recently modified on: April 15, 2020
#
# Notes:
# A) 4/13/20 (IB) - Creates multiple plots in one printout
# B) 4/14/20 (IB) - Consistent size windows of plots
# C) 4/14/20 (IB) - Option to display points on second plot
# D) 4/14/20 (IB) - Options to customize size, character, and color of points
# E) 4/15/20 (IB) - Matched formatting for spatial_data() to spatial_power() output
# ------------------------------------------ #

spatial_plots <- function(input,
                          p_thresh = 0.8,
                          plot_text = FALSE,
                          n_sim = 4,
                          cols = c("grey0", "grey80", "grey100", "red", "blue"),
                          chars = c(1,1),
                          sizes = c(0.1,0.1),
                          plot_pts = TRUE,
                          ...) {
  
  # Packages
  require(fields)
  require(raster)
  require(sp)
  
  if("ppplist" %in% class(input)){
    return(sp::plot(input[1:n_sim], 
             pch = chars,
             
             cex = sizes,
             cols = c(cols[4], cols[5]),
             leg.side = "bottom",
             leg.args = list(cex.axis = 0.9, cex = 1, pch = chars),
             main = "First iteration(s)\nof simulated data"
    ))
  }
  
# Plot 1: Example input
p1 <- spatstat::plot.ppp(input$sim, 
               pch = chars, 
               cex = sizes,
               cols = c(cols[4], cols[5]),
               leg.side = "bottom",
               leg.args = list(cex.axis = 0.9, cex = 1, pch = chars),
               main = "First iterations of simulated data"
               )

# Plot 2: Power, Continuous
## Create proportion significant raster
pvalprop <- as.data.frame(dplyr::tibble(x = input$rx,
                                        y = input$ry,
                                        prop = input$pval_prop
                                        )
                          )
lrr_narm <- na.omit(pvalprop) # remove NAs
sp::coordinates(lrr_narm) <- ~ x + y # coordinates
sp::gridded(lrr_narm) <- TRUE # gridded
pvalprop_raster <- raster::raster(lrr_narm)
pvalprop <- NULL # conserve memory
lrr_narm <- NULL # conserve memory

## Colors for raster
rampcols <- grDevices::colorRampPalette(colors = c(cols[1], cols[2]),
                                        space="Lab"
                                        )(length(raster::values(pvalprop_raster)))
rampbreaks <- seq(pvalprop_raster@data@min,
                  pvalprop_raster@data@max, 
                  length.out = length(raster::values(pvalprop_raster))+1
                  )
## Continuous Output
if(plot_pts == TRUE) {
p2 <- spatstat::plot.ppp(input$sim, 
               pch = chars, 
               cex = sizes,
               cols = c(cols[4], cols[5]),
               leg.side = "bottom",
               leg.args = list(cex.axis = 0.9, cex = 1, pch = chars),
               main = "Local power:\nProportion of simulations significant"
               )
fields::image.plot(pvalprop_raster, 
                   col = rampcols,
                   breaks = rampbreaks,
                   axes = F,
                   cex.lab = 1,
                   xlab = "",
                   ylab = "",
                   cex = 1,
                   bigplot = c(0.25, 0.8, 0.2, 0.8),
                   smallplot = c(0.82, 0.84, 0.32, 0.8),
                   axis.args = list(cex.axis = 0.67),
                   add = T,
                   legend.args = list(text = "Power",
                                      side = 4,
                                      line = 2,
                                      cex = 0.67
                                      )
                   )
spatstat::plot.ppp(input$sim, 
         pch = chars, 
         cex = sizes,
         cols = c(cols[4], cols[5]),
         add = T
         )
} else {
  p2 <- spatstat::plot.ppp(input$sim, 
                           pch = chars, 
                           cex = sizes,
                           cols = c("transparent", "transparent"),
                           leg.side = "bottom",
                           leg.args = list(cex.axis = 0.0000000000000001),
                           main = "Local power:\nProportion of simulations significant"
  )
  fields::image.plot(pvalprop_raster, 
                     col = rampcols,
                     breaks = rampbreaks,
                     axes = F,
                     cex.lab = 1,
                     xlab = "",
                     ylab = "",
                     cex = 1,
                     bigplot = c(0.25, 0.8, 0.2, 0.8),
                     smallplot = c(0.82, 0.84, 0.32, 0.8),
                     axis.args = list(cex.axis = 0.67),
                     add = T,
                     legend.args = list(text = "Power",
                                        side = 4,
                                        line = 2,
                                        cex = 0.67
                     )
  )
}

rampbreaks <- NULL # conserve memory
rampcols <- NULL # convserve memory

# Plot 2: Power, labled
## Reclassify raster of proportion significant
#### Here: power = 80
pvalprop_reclass <- raster::reclassify(pvalprop_raster,
                                       c(-Inf,
                                         p_thresh-0.0000001, 1,
                                         p_thresh-0.0000001, Inf, 2
                                         )
                                       )
pvalprop_raster <- NULL # conserve memory

## Categorical Output
p3 <- spatstat::plot.ppp(input$sim, 
               pch = chars, 
               cex = sizes,
               cols = c("transparent", "transparent"),
               leg.side = "bottom",
               leg.args = list(cex.axis = 0.0000000000000001),
               main = paste("Local power:\nProportion significant above",
                            p_thresh,
                            "threshold",
                            sep = " ")
)
fields::image.plot(pvalprop_reclass, 
                   add = T,
                   col = cols[1:2],
                   breaks = c(1, 1.5, 2),
                   axes = F,
                   cex.lab = 1,
                   xlab = "",
                   ylab = "",
                   cex = 1,
                   bigplot = c(0.25, 0.8, 0.2, 0.8),
                   smallplot = c(0.82, 0.84, 0.32, 0.8),
                   axis.args = list(cex.axis = 0.67,
                                    labels = c("insufficient",
                                               "sufficient"),
                                    at = c(1.25, 1.75)
                                    ),
                   legend.args = list(text = "Power",
                                      side = 4,
                                      line = 2,
                                      cex = 0.67
                                      )
                   )
### Add text of local power to each knot
if(plot_text == T){
  text(x = input$rx, y = input$ry, input$pval_prop, col = cols[3], cex = 0.5)
  }
pvalprop_reclass <- NULL # conserve memory
}
# -------------------- END OF CODE -------------------- #