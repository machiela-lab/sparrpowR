#' Function to Estimate the Power of a Spatial Relative Risk using Simulated Data
#'
#' @param input Use output of SRR simulation
#' @param p_thresh p threshold, default is 0.8
#' @param plot_text default is FALSE, in case resolution >> 10
#' @param n_sim TO ADD
#' @param cols colors, default colors = c("grey0", "grey80", "grey100", "red", "blue")
#' @param chars case, control
#' @param sizes case, control
#' @param plot_pts TO ADD, default is TRUE
#' @param ... TO ADD
#'
#' @return TO ADD, add output of the function here
#' @importFrom stats na.omit
#' @importFrom graphics text
#' @export 
#'
#' @examples
#' \dontrun{
#' # Default colors = c("grey0", "grey80", "grey100", "red", "blue")
#' spatial_plots(input = sim_power, # use output of SRR simulation
#'               p_thresh = 0.8, # default = 0.8
#'               #plot_text = T, # default = FALSE in case resolution >> 10
#'               plot_pts = T, # default = TRUE 
#'               chars = c(4,5), # case, control
#'               sizes = c(0.5,0.5), # case, control
#'               cols = c("#0000ff", "#00ff00", "#ff0000", "#a020f0", "#ffa500") 
#'                  #c("blue", "green", "red", "purple", "orange") 
#'               )
#' }
#' 
spatial_plots <- function(input,
                          p_thresh = 0.8,
                          plot_text = FALSE,
                          n_sim = 4,
                          cols = c("#000000", "#cccccc", "ffffff", "ff0000", "0000ff"),
                          chars = c(1,1),
                          sizes = c(0.1,0.1),
                          plot_pts = TRUE,
                          ...) {
  

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
rampbreaks <- seq(0, 1, 
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