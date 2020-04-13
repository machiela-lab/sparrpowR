# ------------------------------------------ #
# Example of power calculation of a spatial relative risk 
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: April 13, 2020
#
# Recently modified by:
# Recently modified on:
#
# Notes:
# A) 4/13/20 (IB) - Trimmed down example found in "Example_SRR.R"
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
spatial_plots(input = rand_pts) # use output of data simulation

# Estimates SRR with the following arguments:
### upper_tail = user-specified upper tail of a two-tailed significance level
### lower_tail = user-specified lower tail of a two-tailed significance level
### resolution = 10 to calculate surfaces in a 10 x 10 grid
### edge = "diggle" to employ the Diggle method that reweights each observation-specific kernel
### adapt = F to estimate using fixed smoothing (future direction: explore adaptive smoothing)
### h0 = NULL for internal estimation of a common oversmoothing bandwidth computed via the sparr::OS() function in the sparr package (can be user specified if want to force same bandwidth across iterations)
### There are other arguments for tuning (mostly for adaptive smoothing), see sparr::risk() helpfile

## NOTE: Force the sparr::risk() arguement tolerate = TRUE to always calculate asymptotic p-vlaue surfaces
## NOTE: Force the sparr::risk() arguement verbose = FALSE to clean-up presentation 

# Set seed for reproducibility
set.seed(1234)

start_time <- Sys.time() # record start time
sim_srr <- spatial_power(x_case = c(0.25, 0.5, 0.75),
                         y_case = c(0.75, 0.25, 0.75),
                         n_case = c(100, 100, 100),
                         n_control = 700,
                         r_case = c(0.1, 0.1, 0.1),
                         sim_total = 10,
                         samp_case = "uniform",
                         samp_control = "CSR",
                         win = spatstat::unit.square(), # the default
                         upper_tail = 0.995, # default = 0.975
                         lower_tail = 0.005, # default = 0.025
                         resolution = 10, # try the default 128 for a smoother surface
                         edge = "diggle",
                         adapt = F,
                         h0 = NULL
                         )
end_time <- Sys.time() # record end time
time_srr <- end_time - start_time # Calculate run time
time_srr # n = 10,000 about (12 min for version 1; 11 min for version 2)

## Data Visualizaiton of Input and Power
spatial_plots(input = sim_srr, # use output of SRR simulation
              p_thresh = 0.9, # default = 0.8
              plot_text = T # default = FALSE in case resolution >> 10
              )