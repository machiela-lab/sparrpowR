# ------------------------------------------ #
# Example of power calculation of a spatial relative risk for previously collected data
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: April 14, 2020
#
# Recently modified by: @idblr
# Recently modified on: April 30, 2020
#
# Notes:
# A) 04/14/2020 (IB) - Uses 'pbc' data from the 'sparr' package https://doi.org/10.1053/jhep.2001.29760
# B) 04/14/2020 (IB) - Adapted example to match spatial_power() example
# C) 04/14/2020 (IB) - Updated arguments for spatial_plots() function
# D) 04/14/2020 (IB) - Added assessment of sample size of simulated data per iteration
# E) 04/16/2020 (IB) - Added first attempt at adding GIS basemap layer to output plots
# F) 04/16/2020 (IB) - Increased s_control to show effect and extacted bandwidth
# G) 04/30/2020 (IB) - Updated example to demonstrate parallelization
# ------------------------------------------ #

############
# PACKAGES #
############

library(ggmap)
library(maptools)
library(raster)
library(sp)
library(sparr)

####################
# CUSTOM FUNCTIONS #
####################

source(file = "code/R_functions/jitter_power.R")
source(file = "code/R_functions/spatial_plots.R")

#########################################
# ITERATIVE SPATIAL STATISTIC EXAMPLE 2 #
#########################################

# Estimate the mean and standard deviation of the log-relative risk surface 
# Estimate the mean p-value surface and the proportion of iterations that are significant

# Uses previously collected data 
## From the 'sparr' package
data(pbc)

## 3,781 locations
### n = 761 cases
### n = 3,020 controls
### Unique window
### Multivariate normal jittering by N(0,10)
### Power to detect both hot and coldspots

# Estimates SRR with the following arguments:
### sim_total = number of simulation iterations
### samp_control = type of random sampling for controls ('CSR', 'uniform', 'MVN')
### s_control = if MVN, the standard deviation of the random normal noise added to each coordinate of the control locations
### upper_tail = user-specified upper tail of a two-tailed significance level
### lower_tail = user-specified lower tail of a two-tailed significance level
### resolution = 10 to calculate surfaces in a 10 x 10 grid
### edge = "diggle" to employ the Diggle method that reweights each observation-specific kernel
### adapt = F to estimate using fixed smoothing (future direction: explore adaptive smoothing)
### h0 = NULL for internal estimation of a common oversmoothing bandwidth computed via the sparr::OS() function in the sparr package (can be user specified if want to force same bandwidth across iterations)
### cascon = TRUE for power to detect both relative case and control clustering (hot and coldspots)
### There are other arguments for tuning (mostly for adaptive smoothing), see sparr::risk() helpfile

## NOTE: Force the sparr::risk() arguement tolerate = TRUE to always calculate asymptotic p-vlaue surfaces
## NOTE: Force the sparr::risk() arguement verbose = FALSE to clean-up presentation 

# Set seed for reproducibility
set.seed(1234)

# Simulate relative clustering
start_time <- Sys.time() # record start time
sim_power <- jitter_power(obs_data = pbc,
                          sim_total = 100,
                          samp_control = "MVN",
                          parallel = TRUE,
                          s_control = 10, # default = 1
                          upper_tail = 0.995, # default = 0.975
                          lower_tail = 0.005, # default = 0.025
                          resolution = 100, # default = 128
                          edge = "diggle", # default = "uniform"
                          cascon = FALSE # default = FALSE
                          )
end_time <- Sys.time() # record end time
time_pts <- end_time - start_time
time_pts

## Data visualization
### Default colors = c("grey0", "grey80", "grey100", "red", "blue")
spatial_plots(input = sim_power, # use output of SRR simulation
              p_thresh = 0.8, # default = 0.8
              #plot_text = T, # default = FALSE in case resolution >> 10
              plot_pts = T, # default = TRUE 
              chars = c(4,5), # case, control
              sizes = c(0.5,0.5), # case, control
              cols = c("#0000ff", "#00ff00", "#ff0000", "#a020f0", "#ffa500") #c("blue", "green", "red", "purple", "orange") # insufficient, sufficient, text, case, control
)

# Mean and standard devation of samples size of simulated controls
mean(sim_power$n_con); sd(sim_power$n_con)

# Mean and standard devation of bandwidth
mean(sim_power$bandw); sd(sim_power$bandw)

###############################
# ADVANCED DATA VISUALIZATION #
###############################

## Plot with open-source basemap layer
# Convert ppp to SpatialPointsDataFrame
pbc_pts <- maptools::as.SpatialPointsDataFrame.ppp(pbc)
raster::crs(pbc_pts) <- sp::CRS("+init=epsg:27700") # set initial projection as EPSG:27700
pbc_pts@coords <- pbc_pts@coords*1000 # convert units to meters
pbc_pts@bbox <- pbc_pts@bbox*1000 # convert units to meters

# Create and spatially project the boundary of the ppp
p <- sp::Polygon(cbind(pbc$window$bdry[[1]]$x*1000, pbc$window$bdry[[1]]$y*1000)) # create polygon with units in meters
ps <- sp::Polygons(list(p),1) # create polygons
sps <- sp::SpatialPolygons(list(ps)) # conver to SpatialPolygons
raster::crs(sps) <- raster::crs(pbc_pts) # match initial projection as spatial points
sps_wgs84 <-  sp::spTransform(sps, CRS("+init=epsg:4326")) # spatial projection of baselayer
pbcbb <- sp::bbox(sps_wgs84) # bounding box of projected ppp extent
rm(p, ps, sps_wgs84) # conserve memory

# Download basemap
invisible(base_map <- ggmap::get_map(location = pbcbb))

# Convert basemap to raster
mgmap <- as.matrix(base_map) # convert to matrix
vgmap <- as.vector(mgmap) # convert to vector
vgmaprgb <- grDevices::col2rgb(vgmap) # split into red, green, and blue
gmapr <- matrix(vgmaprgb[1, ], ncol = ncol(mgmap), nrow = nrow(mgmap)) # red
gmapg <- matrix(vgmaprgb[2, ], ncol = ncol(mgmap), nrow = nrow(mgmap)) # green
gmapb <- matrix(vgmaprgb[3, ], ncol = ncol(mgmap), nrow = nrow(mgmap)) # blue
rgmaprgb <- raster::brick(raster::raster(gmapr), raster::raster(gmapg), raster::raster(gmapb)) # stack red, green, and blue rasters
rm(gmapr, gmapg, gmapb, vgmap, vgmaprgb, mgmap, pbcbb) # conserve memory
raster::projection(rgmaprgb) <- sp::CRS("+init=epsg:4326") # set initial projection
raster::extent(rgmaprgb) <- unlist(attr(base_map, which = "bb"))[c(2, 4, 1, 3)] # set extent
rgmaprgbGM <- rgmaprgb # duplicate
raster::projection(rgmaprgbGM) <- raster::crs(pbc_pts) # spatial projection of ppp
rprobextSpDF <- as(raster::extent(unlist(attr(base_map, which = "bb"))[c(2, 4, 1, 3)]),
                   "SpatialPolygons") # extract extent
raster::projection(rprobextSpDF) <- raster::crs(rgmaprgb) # set initial projection extent
rprobextGM <- sp::spTransform(rprobextSpDF, raster::crs(pbc_pts)) # spatioal projection of ppp
raster::extent(rgmaprgbGM) <- c(rprobextGM@bbox[1, ], rprobextGM@bbox[2, ]) # set extent
rm(rgmaprgb, rprobextSpDF, base_map) # conserve memory

# Graphing parameters
chars <- c(4,5)
sizes <- c(0.5,0.5)
cols <- c("#0000ff", "#00ff00", "#ff0000", "#a020f0", "#ffa500")

# PLot 1 (Inputs)
raster::plotRGB(rgmaprgbGM)
sp::plot(pbc_pts[pbc_pts$marks == "control",], 
         pch = chars,
         cex = sizes, 
         col = cols[5],
         add = T
)
sp::plot(pbc_pts[pbc_pts$marks == "case",],
         pch = chars,
         cex = sizes, 
         col = cols[4],
         add = T
         )
sp::plot(sps, add = T)

# Plot 2 Prep
pvalprop <- dplyr::tibble(x = sim_power$rx*1000,
                                        y = sim_power$ry*1000,
                                        prop = sim_power$pval_prop
)
lrr_narm <- na.omit(pvalprop) # remove NAs
sp::coordinates(lrr_narm) <- ~ x + y # coordinates
sp::gridded(lrr_narm) <- TRUE # gridded
pvalprop_raster <- raster::raster(lrr_narm)
pvalprop <- NULL # conserve memory
lrr_narm <- NULL # conserve memory
raster::crs(pvalprop_raster) <- raster::crs(rgmaprgbGM)
# pvalprop_raster <- raster::projectRaster(pvalprop_raster, crs = crs(dcwa))

rampcols <- grDevices::colorRampPalette(colors = c(cols[1], cols[2]),
                                        space="Lab"
)(length(raster::values(pvalprop_raster)))
rampbreaks <- seq(0, 1, 
                  length.out = length(raster::values(pvalprop_raster))+1)

# PLot 2 (Continuous Output)
raster::plotRGB(rgmaprgbGM)
sp::plot(pvalprop_raster, 
     add = TRUE,
     ext = extent(rgmaprgbGM),
     col = rampcols,
     alpha = 0.67,
     legend.args = list(text = "Power",
                        side = 4,
                        line = 2,
                        cex = 0.67
     )
)
sp::plot(sps, add = T)

# Plot 3 Prep
p_thresh <- 0.8
pvalprop_reclass <- raster::reclassify(pvalprop_raster,
                                       c(-Inf,
                                         p_thresh-0.0000001, 1,
                                         p_thresh-0.0000001, Inf, 2
                                       )
)

# Plot 3 (Categorical Output)
raster::plotRGB(rgmaprgbGM)
raster::plot(pvalprop_reclass, 
     add = TRUE,
     ext = extent(rgmaprgbGM),
     col = cols[1:2],
     alpha = 0.67,
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
sp::plot(sps, add = T)

# -------------------- END OF CODE -------------------- #