# ------------------------------------------ #
# Example of SRR model using the sparr package
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: April 8, 2020
#
# Recently modified by: 
# Recently modified on:
#
# Notes:
# A) 4/8/20 (IB) - Uses simulated data from the companion file "Random_Clustering.R"
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

source(file = paste(getwd(), "/code/R_functions/rand_cascon_neymscot.R", sep = ""))
source(file = paste(getwd(), "/code/R_functions/lrr_ramp.R", sep = ""))

######################
# EXAMPLE SIMULATION #
######################

# Arguments 
## Prevlance: Proportion of cases
## n_total: Total sample size
## n_case: Approximate sample size of cases within each simulated case cluster
## n_control: Approximate sample size of controls within each simulated control cluster
## k_case: kappa of all case clusters
## k_control: kappa of all control clusters
## e_case: expansion of all case clusters
## e_control: expansion of all control clusters
## r_case: radius of all case clusters
## r_control: radius of all control clusters
## sim_total: number of simulation iterations
## Other arguments passed to rNeymanScott() function in "spatstat" package (e.g., win, lmax)

# Example
## 20% Prevalence
## 1000 total points
## 50 case points per cluster
## 200 case points per cluster
## kappa = 5 for both cases and controls
## expansion = 0 for both cases and controls
## 0.1 units for radii of case clusters
## 0.5 units for radii of control clusters
## within a unit square window (0,1),(0,1)

### NOTE: Sample sizes, prevalence, and kappa are all interlinked and requires tuning

# Set seed for reproducibility
set.seed(1234)

# Simulate relative clustering
rand_pts <- rand_cascon_neymscot(prevalence = 0.2, 
                                 n_total = 1000,
                                 n_case = 50,
                                 n_control = 200,
                                 k_case = 5, 
                                 k_control = 5,
                                 e_case = 0, 
                                 e_control = 0, 
                                 r_case = 0.1, 
                                 r_control = 0.5,
                                 sim_total = 4,
                                 win = spatstat::unit.square()
)

lapply(rand_pts, FUN = function(x) {x$n}) # double check sample size
lapply(rand_pts, FUN = function(x) {table(x$marks)}) # double check prevalence

## Data Visualization
### All iterations
plot(rand_pts, pch = 1, cex = c(0.5,0.1), cols = c("red", "blue"), main = "Random Simulation")

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
     main = "Estimated significant p-values\n alpha = 0.1 and 0.05",
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

# -------------------- END OF CODE -------------------- #