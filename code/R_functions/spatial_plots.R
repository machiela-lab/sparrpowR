# ------------------------------------------ #
# Function to Estimate the Power of a Spatial Relative Risk using Simulated Data
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: April 13, 2020
#
# Recently modified by:
# Recently modified on:
#
# Notes:
# A) 4/13/20 (IB) - Combines rand_cascon() and rand_srr() functions per iteration
# ------------------------------------------ #

spatial_plots <- function(input,
                          p_thresh = 0.8,
                          plot_text = FALSE,
                          n_sim = 4,
                          cols = c("grey0", "grey80", "grey100"),
                          ...) {
  
  # Packages
  require(fields)
  require(raster)
  require(sp)
  
  if("ppplist" %in% class(input)){
    return(sp::plot(input[1:n_sim], 
             pch = 1, 
             cex = c(0.5,0.1),
             cols = c("red", "blue"),
             leg.side = "right",
             main = "First iterations of simulated data"
    ))
  }
  
# Plot 1: Example input
p1 <- sp::plot(input$sim, 
               pch = 1, 
               cex = c(0.5,0.1),
               cols = c("red", "blue"),
               leg.side = "right",
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
p2 <- fields::image.plot(pvalprop_raster, 
                         main = "Local power:\nProportion of simulations significant",
                         col = rampcols,
                         breaks = rampbreaks,
                         axes = F,
                         cex.lab = 1,
                         xlab = "",
                         ylab = "",
                         cex = 1,
                         bigplot = c(0.25, 0.8, 0.2, 0.8),
                         smallplot = c(0.82, 0.84, 0.2, 0.8),
                         axis.args = list(cex.axis = 0.67),
                         legend.args = list(text = "Power",
                                            side = 4,
                                            line = 2,
                                            cex = 0.67
                                            )
                         )
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
p3 <- fields::image.plot(pvalprop_reclass, 
                         main = paste("Local power:\nProportion significant above",
                                      p_thresh,
                                      "threshold",
                                      sep = " "),
                         col = cols[1:2],
                         breaks = c(1, 1.5, 2),
                         axes = F,
                         cex.lab = 1,
                         xlab = "",
                         ylab = "",
                         cex = 1,
                         bigplot = c(0.25, 0.8, 0.2, 0.8),
                         smallplot = c(0.82, 0.84, 0.2, 0.8),
                         axis.args = list(
                           cex.axis = 0.67,
                           labels = c("insufficient", "sufficient"),
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