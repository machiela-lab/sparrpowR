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
# A)
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

# Set seed for reproducibility
set.seed(1234)

# Simulate relative clustering
start_time <- Sys.time() # record start time
sim_power <- jitter_power(obs_data = pbc,
                          sim_total = 100,
                          samp_control = "jitter",
                          scalar = 1,
                          resolution = 128,
                          cascon = T
                          )
time_pts <- end_time - start_time
time_pts

spatial_plots(input = sim_power, # use output of SRR simulation
              p_thresh = 0.8, # default = 0.8
              #plot_text = T, # default = FALSE in case resolution >> 10
              #cols = c("blue", "green", "red") # insufficient, sufficient, text
)
# -------------------- END OF CODE -------------------- #