# ------------------------------------------ #
# Function to create a diverging color ramp at a custom midpoint and upper/lower thresholds
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: April 8, 2020
#
# Recently modified by:
# Recently modified on:
#
# Notes:
# A) 4/8/20 (IB) - Based on 2 years of data visualization fwith the sparr package
# B) 4/8/20 (IB) - Useful for color ramp in plots of log relative risk surface
# ------------------------------------------ #

lrr_ramp <- function(x, cols, midpoint = 0, thresh_up = NULL, thresh_low = NULL) {
  
  # Packages
  require(grDevices)
  
  # Inputs
  if (class(x) != "RasterLayer") {
    stop("The 'x' argument must be of class 'RasterLayer'")
  }
  
  if (length(cols) != 3) {
    stop("The 'cols' argument must be a vector of length 3")
  }
  
  # Restrict spurious log relative risk values
  if (!is.null(thresh_low)) {
    x[x <= thresh_low] <- thresh_low
  }
  if (!is.null(thresh_up)){
    x[x >= thresh_up] <- thresh_up
  }
  
  # Identify ramp above and below midpoint
  lowerhalf <- length(x[x < midpoint & !is.na(x)]) # values below 0
  upperhalf <- length(x[x > midpoint & !is.na(x)]) # values above 0
  nhalf <- length(x[!is.na(x)])/2 # number of values at half
  min_absolute_value <- min(x[is.finite(x)], na.rm = T) # minimum absolute value of raster
  max_absolute_value <- max(x[is.finite(x)], na.rm = T) # maximum absolute value of raster
  
  # Color ramp parameters
  ## Colors
  ### vector of colors for values below midpoint
  rc1 <- grDevices::colorRampPalette(colors = c(cols[3], cols[2]), space="Lab")(lowerhalf)
  ### vector of colors for values above midpoint
  rc2 <- grDevices::colorRampPalette(colors = c(cols[2], cols[1]), space="Lab")(upperhalf)
  ### compile colors
  rampcols <- c(rc1, rc2) 
  ### add midpoint color
  #rampcols[c(lowerhalf+1,upperhalf)] <- grDevices::rgb(t(col2rgb(cols[2])), maxColorValue = 256) 
  ## Breaks
  ### vector of breaks for values below midpoint
  rb1 <- seq(min_absolute_value, midpoint, length.out = lowerhalf+1) 
  ### vector of breaks for values above midpoint
  rb2 <- seq(midpoint, max_absolute_value, length.out = upperhalf+1)[-1] 
  ### compile breaks
  rampbreaks <- c(rb1, rb2)
  
  # Output
  out <- list("lrr" = x,
              "cols" = rampcols,
              "breaks" = rampbreaks
              )
  
}
# -------------------- END OF CODE -------------------- #