# ------------------------------------------ #
# Simulate and Combine Two Separate Marked Neyman-Scott Cluster Processes
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: April 7, 2020
#
# Recently modified by: @idblr
# Recently modified on: April 8, 2020
#
# Notes:
# A) 4/7/20 (IB) - Potential simulation scheme for sparrpowR package 0.0.0.9000
# B) 4/7/20 (IB) - Simulates a Neyman-Scott Cluster Process for cases and a separate Neyman-Scott Cluster Process for controls
# C) 4/7/20 (IB) - rNeymanScott helpfile: https://www.rdocumentation.org/packages/spatstat/versions/1.62-2/topics/rNeymanScott
# D) 4/7/20 (IB) - Allows functionality for user-specified prevalence 
# E) 4/7/20 (IB) - Allows functionality for user-specified, consistent sample sizes 
# F) 4/8/20 (IB) - Added functionality for iterative simulations
# G) 4/8/20 (IB) - Moved custom function to companion file "/R_functions/rand_cascon_neymscot.R"
# H) 4/8/20 (IB) - Added functionality for uniform random sampling of controls with consistent case clusters of uniform disc of user-specified radii and centroids. Companion file "/R_functions/rand_cascon_unifdisc.R"
# ------------------------------------------ #

############
# PACKAGES #
############

# install.packages("spatstat")
library(spatstat)

###################
# CUSTOM FUNCTION #
###################

source(file = paste(getwd(), "/code/R_functions/rand_cascon_neymscot.R", sep = ""))
source(file = paste(getwd(), "/code/R_functions/rand_cascon_unifdisc.R", sep = ""))

#######################
# EXAMPLE SIMULATIONS #
#######################

# Example 1: Random cases and random controls using two Neyman-Scott Processes

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
## four simulation iterations

### NOTE: Sample sizes, prevalence, and kappa are all interlinked and requires tuning

# Set seed for reproducibility
set.seed(1234)

# Simulate relative clustering
rand_pts1 <- rand_cascon_neymscot(prevalence = 0.2, 
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

lapply(rand_pts1, FUN = function(x) {x$n}) # double check sample size
lapply(rand_pts1, FUN = function(x) {table(x$marks)}) # double check prevalence

## Data Visualization
plot(rand_pts1, pch = 1, cex = c(0.5,0.1), cols = c("red", "blue"), main = "Random Neyman-Scott Simulation")

# Example 2: Consistent random uniform disc of cases and complete spatial random controls

# Arguments 
## x_case: x-coordinate of case cluster centroid
## y_case: y-coordinate of case cluster centroid
## n_case: Sample size of cases within the simulated case cluster
## n_control: Sample size of controls within window
## r_case: radius of case cluster
## sim_total: number of simulation iterations
## type_sampling: selection of sampling schemes for control points
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
rand_pts2 <- rand_cascon_unifdisc(x_case = c(0.25, 0.5, 0.75),
                             y_case = c(0.75, 0.25, 0.75),
                             n_case = c(100, 100, 100),
                             n_control = 700,
                             r_case = c(0.1, 0.1, 0.1),
                             sim_total = 4,
                             type_sampling = "CSR",
                             win = spatstat::unit.square()
)

lapply(rand_pts2, FUN = function(x) {x$n}) # double check sample size
lapply(rand_pts2, FUN = function(x) {table(x$marks)}) # double check prevalence

## Data Visualization
plot(rand_pts2, pch = 1, cex = c(0.5,0.1), cols = c("red", "blue"), main = "CSR Uniform Simulation")

# Example 3: Consistent random uniform disc of cases and complete spatial random controls

# Arguments 
## Same as in Example 2

# Example
## 1000 total points
## 3 case clusters
## 100 case points within each case cluster (300 case points total)
## Approximately 700 control points within window (stratified)
## 0.1 units for radius of each case cluster
## within a unit square window (0,1),(0,1)
## case cluster centroids located at (0.25,0.75), (0.5,0.25), & (0.75,0.75)
## four simulation iterations (only control locations change between iteration)

## NOTE: Cannot restrict to a consistent sample size of control points

# Set seed for reproducibility
set.seed(1234)

# Simulate relative clustering
rand_pts3 <- rand_cascon_unifdisc(x_case = c(0.25, 0.5, 0.75),
                                  y_case = c(0.75, 0.25, 0.75),
                                  n_case = c(100, 100, 100),
                                  n_control = 700,
                                  r_case = c(0.1, 0.1, 0.1),
                                  sim_total = 4,
                                  type_sampling = "stratified",
                                  win = spatstat::unit.square()
)

lapply(rand_pts3, FUN = function(x) {x$n}) # double check sample size
lapply(rand_pts3, FUN = function(x) {table(x$marks)}) # double check prevalence

## Data Visualization
plot(rand_pts3, pch = 1, cex = c(0.5,0.1), cols = c("red", "blue"), main = "Stratified Uniform Simulation")

# -------------------- END OF CODE -------------------- #