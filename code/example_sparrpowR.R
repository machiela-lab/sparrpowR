# ------------------------------------------ #
# Example of power calculation of a spatial relative risk for simulated data
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: April 13, 2020
#
# Recently modified by: @idblr
# Recently modified on: April 15, 2020
#
# Notes:
# A) 4/13/20 (IB) - Trimmed down example found in "Example_SRR.R"
# B) 4/15/20 (IB) - Specified arguments for full spatial_power() function
# C) 4/15/20 (IB) - Match example for spatial_data() and spatial_power()
# ------------------------------------------ #

####################
# CUSTOM FUNCTIONS #
####################

source(file = paste(getwd(), "/code/R_functions/spatial_data.R", sep = ""))
source(file = paste(getwd(), "/code/R_functions/spatial_power.R", sep = ""))
source(file = paste(getwd(), "/code/R_functions/spatial_plots.R", sep = ""))

#########################################
# ITERATIVE SPATIAL STATISTIC EXAMPLE 2 #
#########################################

# Estimate the mean and standard deviation of the log-relative risk surface 
# Estimate the mean p-value surface and the proportion of iterations that are significant

# Uses data simulated with the following arugments
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
start_time <- Sys.time() # record start time
rand_pts <- spatial_data(x_case = c(0.25, 0.5, 0.75),
                         y_case = c(0.75, 0.25, 0.75),
                         n_case = c(100, 100, 100),
                         n_control = 700,
                         r_case = c(0.1, 0.1, 0.1),
                         sim_total = 10,
                         samp_case = "uniform",
                         samp_control = "CSR",
                         win = spatstat::unit.square()
)
end_time <- Sys.time() # record end time
time_pts <- end_time - start_time # n = 10,000 about 6 min
time_pts

## Data Visualizaiton of Input and Power
spatial_plots(input = rand_pts, # use output of data simulation
              n_sim = 2, # default = 4 simulations
              chars = c(4,5), # case, control
              sizes = c(0.5,0.5), # case, control
              cols = c("blue", "green", "red", "purple", "orange") # insufficient, sufficient, text, case, control
              ) 

# Estimates SRR with the following arguments:
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

# Lambda for the inhomogenous Poisson process of control locations
l_cont <- function(x, y) {1000 * exp(-3 * x) + 1000 * exp(-3 * y)}
unit.circle <- spatstat::disc(radius = 0.5, centre = c(0.5,0.5))

start_time <- Sys.time() # record start time
sim_srr <- spatial_power(x_case = c(0.25, 0.5, 0.75),
                         y_case = c(0.75, 0.25, 0.75),
                         n_case = c(100, 100, 100),
                         n_control = 700,
                         r_case = c(0.1, 0.1, 0.1),
                         sim_total = 10,
                         samp_case = "uniform",
                         samp_control = "clustered",
                         #l_control = l_cont,
                         l_control = 50,
                         e_control = 0,
                         r_control = 0.01,
                         n_cluster = 10,
                         win = unit.circle, # the default
                         upper_tail = 0.995, # default = 0.975
                         lower_tail = 0.005, # default = 0.025
                         resolution = 50, # default = 128
                         edge = "diggle", # default = "uniform"
                         #adapt = FALSE,
                         #h0 = NULL,
                         cascon = TRUE # cascon = FALSE for only relative case clustering (hotspots)
                         ) 
end_time <- Sys.time() # record end time
time_srr <- end_time - start_time # Calculate run time
time_srr # n = 10,000 about (12 min for version 1; 11 min for version 2)

## Data Visualizaiton of Input and Power
### Default colors = c("grey0", "grey80", "grey100", "red", "blue")
spatial_plots(input = sim_srr, # use output of SRR simulation
              p_thresh = 0.8, # default = 0.8
              #plot_text = T, # default = FALSE in case resolution >> 10
              plot_pts = T, # default = TRUE 
              chars = c(4,5), # case, control
              sizes = c(0.5,0.5), # case, control
              cols = c("blue", "green", "red", "purple", "orange") # insufficient, sufficient, text, case, control
)

# Mean and standard deviation of simulated controls and cases
mean(sim_srr$n_con); sd(sim_srr$n_con) # controls
mean(sim_srr$n_cas); sd(sim_srr$n_cas) # cases

# -------------------- END OF CODE -------------------- #