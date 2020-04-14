# ------------------------------------------ #
# Example of power calculation of a spatial relative risk for previously collected data
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: April 14, 2020
#
# Recently modified by:
# Recently modified on:
#
# Notes:
# A) Uses data from the 'sparr' package
# ------------------------------------------ #

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
### Multivariate normal jittering by N(0,1)
### Power to detect both hot and coldspots

# Estimates SRR with the following arguments:
### sim_total = number of simulation iterations
### samp_control = type of random sampling for controls ('CSR', 'uniform', 'jittered')
### scalar = if jittered, the standard deviation of the random normal noise added to each coordinate of the control locations
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
                          samp_control = "jitter",
                          scalar = 1, # default = 1
                          upper_tail = 0.995, # default = 0.975
                          lower_tail = 0.005, # default = 0.025
                          resolution = 100, # default = 128
                          edge = "diggle", # default = "uniform"
                          cascon = TRUE # default = FALSE
                          )
time_pts <- end_time - start_time
time_pts

spatial_plots(input = sim_power, # use output of SRR simulation
              p_thresh = 0.8, # default = 0.8
              #plot_text = T, # default = FALSE in case resolution >> 10
              #cols = c("blue", "green", "red") # insufficient, sufficient, text
)
# -------------------- END OF CODE -------------------- #