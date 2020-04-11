# ------------------------------------------ #
# Example of SRR model using the sparr package
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: April 8, 2020
#
# Recently modified by: @idblr
# Recently modified on: April 10, 2020
#
# Notes:
# A) 4/8/20 (IB) - Uses simulated data from the companion file "Random_Clustering.R"
# A) 4/10/20 (IB) - Added example calculating summary of iterative simulation 
# ------------------------------------------ #

############
# PACKAGES #
############

# install.pacakges("dplyr")
# install.packages("fields")
# install.packages("raster")
# install.packages("sp")
# install.packages("spatstat")
# install.packages("sparr")
library(dplyr)
library(fields)
library(raster)
library(sp)
library(spatstat)
library(sparr)

####################
# CUSTOM FUNCTIONS #
####################

source(file = paste(getwd(), "/code/R_functions/rand_cascon_unifdisc.R", sep = ""))
source(file = paste(getwd(), "/code/R_functions/lrr_ramp.R", sep = ""))
source(file = paste(getwd(), "/code/R_functions/rand_srr.R", sep = ""))

######################
# EXAMPLE SIMULATION #
######################

# Arguments 
## x_case: x-coordinate of case cluster centroid
## y_case: y-coordinate of case cluster centroid
## n_case: Sample size of cases within the simulated case cluster
## n_control: Sample size of controls within window
## r_case: radius of case cluster
## sim_total: number of simulation iterations
## samp_case: selection of sampling scheme for case locations
## samp_control: selection of sampling scheme for control locations
## Other arguments passed to runifdisc() and rpoint() functions in "spatstat" package (e.g., win)

# Example
## 1000 total points
## 3 case clusters
## 100 case points within each case cluster (300 case points total)
## 700 control points within window (complete spatial randomness)
## 0.1 units for radius of each case cluster
## within a unit square window (0,1),(0,1)
## case cluster centroids located at (0.25,0.75), (0.5,0.25), & (0.75,0.75)
## four simulation iterations (only control locations change between iteration)

# Set seed for reproducibility
set.seed(1234)

# Simulate relative clustering
rand_pts <- rand_cascon(x_case = c(0.25, 0.5, 0.75),
                        y_case = c(0.75, 0.25, 0.75),
                        n_case = c(100, 100, 100),
                        n_control = 700,
                        r_case = c(0.1, 0.1, 0.1),
                        sim_total = 100,
                        samp_case = "uniform",
                        samp_control = "CSR",
                        win = spatstat::unit.square()
)

lapply(rand_pts, FUN = function(x) {x$n}) # double check sample size
lapply(rand_pts, FUN = function(x) {table(x$marks)}) # double check prevalence

## Data Visualization
plot(rand_pts[1:4], pch = 1, cex = c(0.5,0.1), cols = c("red", "blue"),
     main = "CSR Sampling Simulation"
)

#############################
# EXAMPLE SPATIAL STATISTIC #
#############################

# Estimate spatial relative risk function based on the ratio of two 2D kernel density estimates
## Arguments:
### tolerate = T to calculate asymptotic p-vlaue surface
### resolution = 10 to calculate surfaces in a 10 x 10 grid
### edge = "diggle" to employ the Diggle method that reweights each observation-specific kernel
### adapt = F to estimate using fixed smoothing (future direction: explore adaptive smoothing)
### h0 = NULL for internal estimation of a common oversmoothing bandwidth computed via the sparr::OS() function in the sparr package (can be user specified if want to force same bandwidth across iterations)
### verbose = F to clean-up presentation 
### There are other arguments for tuning (mostly for adaptive smoothing), see sparr::risk() helpfile

obs_lrr <- sparr::risk(rand_pts[[1]], 
                       tolerate = T, 
                       resolution = 10, # try the default 128 for a smoother surface
                       edge = "diggle", 
                       adapt = F, 
                       h0 = NULL, 
                       verbose = F
)

# Output processing for visualization
## Convert output matrix to two output rasters
### Coordinates of raster grid
rx <- rep(obs_lrr$rr$xcol, length(obs_lrr$rr$yrow))
for(i in 1:length(obs_lrr$rr$yrow)){
  if (i == 1){ry <- rep(obs_lrr$rr$yrow[i], length(obs_lrr$rr$xcol))}
  if (i != 1){ry <- c(ry,rep(obs_lrr$rr$yrow[i], length(obs_lrr$rr$xcol)))}
}

### Create raster of log relative risk surface
out_lrr <- as.data.frame(dplyr::tibble(x = rx,
                                       y = ry,
                                       lrr = as.vector(t(obs_lrr$rr$v))
)
)
sp::coordinates(out_lrr) <- ~ x + y
sp::gridded(out_lrr) <- TRUE
lrr_raster <- raster::raster(out_lrr)
#plot(lrr_raster) # check raster creation

### Create raster of asymptotic p-value surface
out_pvl <- as.data.frame(dplyr::tibble(x = rx,
                                       y = ry,
                                       lrr = as.vector(t(obs_lrr$P$v))
)
)
sp::coordinates(out_pvl) <- ~ x + y
sp::gridded(out_pvl) <- TRUE
pvl_raster <- raster::raster(out_pvl)
#plot(pvl_raster) # check raster creation

### Reclassify raster of asymptotic p-value surface
#### Here: two alpha levels, both two-tailed
#### 0.1 and 0.05
pvl_raster_reclass <- raster::reclassify(pvl_raster, c(-Inf, 0.005, 1,
                                                       0.005, 0.025, 2,
                                                       0.025, 0.975, 3,
                                                       0.975, 0.995, 4,
                                                       0.995, Inf, 5
)
)
#plot(pvl_raster_reclass) # check raster reclassification

## Data Visualization
### Input
plot(rand_pts[[1]], pch = 1, cex = c(0.5,0.1), cols = c("red", "blue"), main = "Random Simulation")

### Continuous output
#### Colors for rasters
plot_cols5 <- c("indianred4", "indianred1", "grey80","cornflowerblue","blue3")
plot_cols3 <- plot_cols5[-c(2,4)] # just the two extreme colors and the color for insignificance

ramp <- lrr_ramp(x = lrr_raster, 
                 cols = plot_cols3,
                 midpoint = 0, # for a log relative risk midpoint is 0
                 thresh_up = 5, # likely not necessary because max < thresh_up
                 thresh_low = -5
                 )

#### Plot
fields::image.plot(ramp$lrr, 
                   main = "Estimated log relative risk surface",
                   col = ramp$cols, 
                   breaks = ramp$breaks,
                   axes = F,
                   cex.lab = 1,
                   xlab = "",
                   ylab = "",
                   cex = 1,
                   smallplot = c(0.86, 0.88, 0.2, 0.8),
                   axis.args = list(labels = c(paste("<", format(round(min(ramp$lrr[!is.infinite(ramp$lrr)], na.rm = T), digits = 2), nsmall = 1) ,sep = ""),
                                               format(round(min(ramp$lrr[!is.infinite(ramp$lrr)], na.rm=T)/2, digits = 2), nsmall = 1),
                                               0,
                                               format(round(max(ramp$lrr[!is.infinite(ramp$lrr)], na.rm=T)/2, digits = 2), nsmall = 1),
                                               paste(">",format(round(max(ramp$lrr[!is.infinite(ramp$lrr)], na.rm=T), digits = 2), nsmall = 1),sep = "")
                   ),
                   at = c(min(ramp$lrr[!is.infinite(ramp$lrr)], na.rm=T),
                          min(ramp$lrr[!is.infinite(ramp$lrr)], na.rm=T)/2,
                          0,
                          max(ramp$lrr[!is.infinite(ramp$lrr)], na.rm=T)/2,
                          max(ramp$lrr[!is.infinite(ramp$lrr)], na.rm=T)
                   ),
                   cex.axis = 0.67
                   ),
                   legend.args = list(text = "log relative risk", side = 4, line = 2,  cex = 0.67)
)

### Categorical output
plot(pvl_raster_reclass,
     main = "Estimated significant p-values\n alpha = 0.1 and alpha = 0.05",
     col = plot_cols5,
     axes = F,
     bty = "o",
     box = F,
     axis.args = list(at = seq(1,5,1),
                      labels = c("<0.005", "<0.025", "insignficant",
                                 ">0.975",  ">0.995"
                                 ), 
                      cex.axis = 0.5
                   )
     )


### Continuous output with p-value contours
fields::image.plot(ramp$lrr, 
                   main = "Estimated log relative risk surface\nwith asymptotic p-value contours",
                   col = ramp$cols, 
                   breaks = ramp$breaks,
                   axes = F,
                   cex.lab = 1,
                   xlab = "",
                   ylab = "",
                   cex = 1,
                   smallplot = c(0.86, 0.88, 0.2, 0.8),
                   axis.args = list(labels = c(paste("<", format(round(min(ramp$lrr[!is.infinite(ramp$lrr)], na.rm = T), digits = 2), nsmall = 1) ,sep = ""),
                                               format(round(min(ramp$lrr[!is.infinite(ramp$lrr)], na.rm=T)/2, digits = 2), nsmall = 1),
                                               0,
                                               format(round(max(ramp$lrr[!is.infinite(ramp$lrr)], na.rm=T)/2, digits = 2), nsmall = 1),
                                               paste(">",format(round(max(ramp$lrr[!is.infinite(ramp$lrr)], na.rm=T), digits = 2), nsmall = 1),sep = "")
                   ),
                   at = c(min(ramp$lrr[!is.infinite(ramp$lrr)], na.rm=T),
                          min(ramp$lrr[!is.infinite(ramp$lrr)], na.rm=T)/2,
                          0,
                          max(ramp$lrr[!is.infinite(ramp$lrr)], na.rm=T)/2,
                          max(ramp$lrr[!is.infinite(ramp$lrr)], na.rm=T)
                   ),
                   cex.axis = 0.67
                   ),
                   legend.args = list(text = "log relative risk", side = 4, line = 2,  cex = 0.67)
)

## add contours for two alpha levels, two-tailed (0.1 and 0.05)
contour(x = obs_lrr$P$xcol,
        y = obs_lrr$P$yrow,
        z = t(obs_lrr$P$v),
        add = T,
        levels = c(0.005,0.025,0.5,0.975,0.995),
        drawlabels = F,
        col = "black",
        lwd = c(1,2,3,2,1),
        lty = c(2,2,1,3,3)
        )

## add legend for contours
legend("bottom", inset = -0.2, ncol = 2, xpd = T,
       legend = c("p-value < 0.005",
                  "p-value < 0.025",
                  "p-value = 0.5",
                  "p-value > 0.975",
                  "p-value > 0.995"),
       col = "black",
       lwd = c(1,2,3,2,1), 
       lty = c(2,2,1,3,3),
       bty = "n", 
       cex = 0.8,
       pt.cex = 1
)

###############################
# ITERATIVE SPATIAL STATISTIC #
###############################

# Estimate the mean and standard deviation of the log-relative risk surface 
# Estimate the mean p-value surface and the proportion of iterations that are significant

## Arguments:
### sim_locs = the pppList of simulated marked planar point patterns
### upper_tail = user-specified upper tail of a two-tailed significance level
### lower_tail = user-specified lower tail of a two-tailed significance level
### resolution = 10 to calculate surfaces in a 10 x 10 grid
### edge = "diggle" to employ the Diggle method that reweights each observation-specific kernel
### adapt = F to estimate using fixed smoothing (future direction: explore adaptive smoothing)
### h0 = NULL for internal estimation of a common oversmoothing bandwidth computed via the sparr::OS() function in the sparr package (can be user specified if want to force same bandwidth across iterations)
### verbose = F to clean-up presentation 
### There are other arguments for tuning (mostly for adaptive smoothing), see sparr::risk() helpfile

## NOTE: Force the sparr::risk() arguement tolerate = TRUE to always calculate asymptotic p-vlaue surfaces

sim_srr <- rand_srr(sim_locs = rand_pts, 
                    # upper_tail = 0.975, # the default value
                    # lower_tail = 0.025, # the default value
                    upper_tail = 0.995,
                    lower_tail = 0.005, 
                    resolution = 10, # try the default 128 for a smoother surface
                    edge = "diggle", 
                    adapt = F, 
                    h0 = NULL, 
                    verbose = F
                    )

# Create mean log relative risk raster
rr <- as.data.frame(dplyr::tibble(
  x = sim_srr$rx,
  y = sim_srr$ry,
  rr = sim_srr$rr_mean
))
lrr_narm <- na.omit(rr) # remove NAs
sp::coordinates(lrr_narm) <- ~ x + y # coordinates
sp::gridded(lrr_narm) <- TRUE # gridded
rr_raster <- raster::raster(lrr_narm)
#plot(rr_raster) # check raster creation

# Create mean p-value raster
pval <- as.data.frame(dplyr::tibble(
  x = sim_srr$rx,
  y = sim_srr$ry,
  tol = sim_srr$pval_mean
))
lrr_narm <- na.omit(pval) # remove NAs
sp::coordinates(lrr_narm) <- ~ x + y # coordinates
sp::gridded(lrr_narm) <- TRUE # gridded
pval_raster <- raster::raster(lrr_narm)
#plot(pval_raster) # check raster creation
## Reclassify raster of asymptotic p-value surface
#### Here: two alpha levels, both two-tailed
#### 0.1 and 0.05
pval_reclass <- raster::reclassify(pval_raster, c(-Inf, 0.005, 1,
                                                  0.005, 0.025, 2,
                                                  0.025, 0.975, 3,
                                                  0.975, 0.995, 4,
                                                  0.995, Inf, 5
                                                  )
                                   )

# Create standard deviation of log relative risk raster
rrsd <- as.data.frame(dplyr::tibble(
  x = sim_srr$rx,
  y = sim_srr$ry,
  sd = sim_srr$rr_sd
))
lrr_narm <- na.omit(rrsd) # remove NAs
sp::coordinates(lrr_narm) <- ~ x + y # coordinates
sp::gridded(lrr_narm) <- TRUE # gridded
rrsd_raster <- raster::raster(lrr_narm)
#plot(rrsd_raster) # check raster creation

# Create proportion significant raster
pvalprop <- as.data.frame(dplyr::tibble(
  x = sim_srr$rx,
  y = sim_srr$ry,
  prop = sim_srr$pval_prop
))
lrr_narm <- na.omit(pvalprop) # remove NAs
sp::coordinates(lrr_narm) <- ~ x + y # coordinates
sp::gridded(lrr_narm) <- TRUE # gridded
pvalprop_raster <- raster::raster(lrr_narm)
#plot(pvalprop_raster) # check raster creation
## Reclassify raster of proportion significant
#### Here: power = 80
p_thresh <- 0.9
pvalprop_reclass <- raster::reclassify(pvalprop_raster, c(-Inf, p_thresh, 1,
                                                          p_thresh, Inf, 2
                                                          )
                                       )

## Data Visualization

### Mean log relative risk
#### Colors for raster
plot_cols5 <- c("indianred4", "indianred1", "grey80","cornflowerblue","blue3")
plot_cols3 <- plot_cols5[-c(2,4)] # just the two extreme colors and the color for insignificance

ramp <- lrr_ramp(x = rr_raster, 
                 cols = plot_cols3,
                 midpoint = 0, # for a log relative risk midpoint is 0
                 thresh_up = 5, # likely not necessary because max < thresh_up
                 thresh_low = -5
                 )
#### Plot
fields::image.plot(ramp$lrr, 
                   main = "Mean log relative risk",
                   col = ramp$cols, 
                   breaks = ramp$breaks,
                   axes = F,
                   cex.lab = 1,
                   xlab = "",
                   ylab = "",
                   cex = 1,
                   smallplot = c(0.86, 0.88, 0.2, 0.8),
                   axis.args = list(labels = c(paste("<", format(round(min(ramp$lrr[!is.infinite(ramp$lrr)], na.rm = T), digits = 2), nsmall = 1) ,sep = ""),
                                               format(round(min(ramp$lrr[!is.infinite(ramp$lrr)], na.rm=T)/2, digits = 2), nsmall = 1),
                                               0,
                                               format(round(max(ramp$lrr[!is.infinite(ramp$lrr)], na.rm=T)/2, digits = 2), nsmall = 1),
                                               paste(">",format(round(max(ramp$lrr[!is.infinite(ramp$lrr)], na.rm=T), digits = 2), nsmall = 1),sep = "")
                   ),
                   at = c(min(ramp$lrr[!is.infinite(ramp$lrr)], na.rm=T),
                          min(ramp$lrr[!is.infinite(ramp$lrr)], na.rm=T)/2,
                          0,
                          max(ramp$lrr[!is.infinite(ramp$lrr)], na.rm=T)/2,
                          max(ramp$lrr[!is.infinite(ramp$lrr)], na.rm=T)
                   ),
                   cex.axis = 0.67
                   ),
                   legend.args = list(text = "mean log relative risk", side = 4, line = 2,  cex = 0.67)
)

### Standard deviation log relative risk
#### Colors
rampcols <- terrain.colors(length(raster::values(rrsd_raster)))
rampbreaks <- seq(rrsd_raster@data@min, rrsd_raster@data@max, 
                  length.out = length(raster::values(rrsd_raster))+1
)
#### Plot
fields::image.plot(rrsd_raster, 
                   main = "Standard deviation log relative risk",
                   col = rampcols, 
                   breaks = rampbreaks,
                   axes = F,
                   cex.lab = 1,
                   xlab = "",
                   ylab = "",
                   cex = 1,
                   smallplot = c(0.86, 0.88, 0.2, 0.8),
                   axis.args = list(labels = format(round(seq(rrsd_raster@data@min,
                                                   rrsd_raster@data@max,
                                                   (1/6)*rrsd_raster@data@max
                                                   ), digits = 2), nsmall = 1),
                                    at =  seq(rrsd_raster@data@min,
                                              rrsd_raster@data@max,
                                              (1/6)*rrsd_raster@data@max
                                              ),
                                    cex.axis = 0.67
                                    ),
                   legend.args = list(text = "standard deviation log relative risk",
                                      side = 4, line = 2,  cex = 0.67
                                      )
                   )

### Mean p-value
### Categorical output
plot(pval_reclass,
     main = "Significant mean p-values\n alpha = 0.1 and alpha = 0.05",
     col = plot_cols5,
     axes = F,
     bty = "o",
     box = F,
     axis.args = list(at = seq(1,5,1),
                      labels = c("<0.005", "<0.025", "insignficant",
                                 ">0.975",  ">0.995"
                      ), 
                      cex.axis = 0.5
                      )
     )

### Proportion of p-value significant
#### Colors for raster
rampcols <- rev(gray.colors(length(raster::values(pvalprop_raster)), 
                            start = 0.95, end = 0, 
                            gamma = 1, alpha = NULL
                            )
                )
rampbreaks <- seq(pvalprop_raster@data@min, pvalprop_raster@data@max, 
                  length.out = length(raster::values(pvalprop_raster))+1
                  )
#### Continuous Output
fields::image.plot(pvalprop_raster, 
                   main = "Proportion significant",
                   col = rampcols, 
                   breaks = rampbreaks,
                   axes = F,
                   cex.lab = 1,
                   xlab = "",
                   ylab = "",
                   cex = 1,
                   smallplot = c(0.86, 0.88, 0.2, 0.8),
                   axis.args = list(cex.axis = 0.67),
                   legend.args = list(text = "proportion significant",
                                      side = 4, line = 2,  cex = 0.67
                                      )
                   )

#### Categorical output
plot(pvalprop_reclass,
     main = paste("Proportion significant above", p_thresh,"threshold", sep = " "),
     col = c("grey", "black"),
     axes = F,
     bty = "o",
     box = F,
     axis.args = list(at = c(1,2),
                      labels = c("insufficient",
                                 "sufficient"
                      ), 
                      cex.axis = 0.5
     )
)
# -------------------- END OF CODE -------------------- #