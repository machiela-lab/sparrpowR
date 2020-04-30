# ------------------------------------------ #
# Simulate and Combine Two Separate Marked Neyman-Scott Cluster Processes
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: April 7, 2020
#
# Recently modified by: @idblr
# Recently modified on: April 9, 2020
#
# Notes:
# A) 4/7/20 (IB) - Potential simulation scheme for sparrpowR package 0.0.0.9000
# B) 4/7/20 (IB) - Simulates a Neyman-Scott Cluster Process for cases and a separate Neyman-Scott Cluster Process for controls
# C) 4/7/20 (IB) - rNeymanScott helpfile: https://www.rdocumentation.org/packages/spatstat/versions/1.62-2/topics/rNeymanScott
# D) 4/7/20 (IB) - Allows functionality for user-specified prevalence 
# E) 4/7/20 (IB) - Allows functionality for user-specified, consistent sample sizes 
# F) 4/8/20 (IB) - Added functionality for iterative simulations
# G) 4/8/20 (IB) - Moved custom function to companion file "/R_functions/rand_cascon_neymscot.R"
# H) 4/8/20 (IB) - Added functionality for uniform random sampling of controls (via various sampling schemes) with consistent case clusters of uniform disc of user-specified radii and centroids. Companion file "/R_functions/rand_cascon_unifdisc.R"
# I) 4/9/20 (IB) - Added examples for the various random control location sampling
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
plot(rand_pts1, pch = 1, cex = c(0.5,0.1), cols = c("red", "blue"),
     main = "Random Neyman-Scott Simulation"
     )

# Example 2: Consistent random uniform disc of cases and complete spatial random controls

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
rand_pts2 <- rand_cascon(x_case = c(0.25, 0.5, 0.75),
                         y_case = c(0.75, 0.25, 0.75),
                         n_case = c(100, 100, 100),
                         n_control = 700,
                         r_case = c(0.1, 0.1, 0.1),
                         sim_total = 4,
                         samp_case = "uniform",
                         samp_control = "CSR",
                         win = spatstat::unit.square()
                         )

lapply(rand_pts2, FUN = function(x) {x$n}) # double check sample size
lapply(rand_pts2, FUN = function(x) {table(x$marks)}) # double check prevalence

## Data Visualization
plot(rand_pts2, pch = 1, cex = c(0.5,0.1), cols = c("red", "blue"),
     main = "CSR Sampling Simulation"
     )

# Example 3: Consistent random uniform disc of cases and systematic random controls

# Arguments 
## Same as in Example 2

# Example
## Approximately 1000 total points
## 3 case clusters
## 100 case points within each case cluster (300 case points total)
## Approximately 700 control points within window (systematic random)
## 0.1 units for radius of each case cluster
## within a unit square window (0,1),(0,1)
## case cluster centroids located at (0.25,0.75), (0.5,0.25), & (0.75,0.75)
## four simulation iterations (only control locations change between iteration)

## NOTE: Cannot easily restrict to a consistent sample size of control points

# Set seed for reproducibility
set.seed(1234)

# Simulate relative clustering
rand_pts3 <- rand_cascon(x_case = c(0.25, 0.5, 0.75),
                         y_case = c(0.75, 0.25, 0.75),
                         n_case = c(100, 100, 100),
                         n_control = 700,
                         r_case = c(0.1, 0.1, 0.1),
                         sim_total = 4,
                         samp_case = "uniform",
                         samp_control = "systematic",
                         win = spatstat::unit.square()
                         )

lapply(rand_pts3, FUN = function(x) {x$n}) # double check sample size
lapply(rand_pts3, FUN = function(x) {table(x$marks)}) # double check prevalence

## Data Visualization
plot(rand_pts3, pch = 1, cex = c(0.5,0.1), cols = c("red", "blue"), 
     main = "Stratified Uniform Simulation"
     )

# Example 4: Consistent random uniform disc of cases and stratified random controls

# Arguments 
## Same as in Example 2
## n_knot: total number of knots (gridded) within which points are randomly generated independently 

# Example
## Approximately 1000 total points
## 3 case clusters
## 100 case points within each case cluster (300 case points total)
## Approximately 700 control points within window (stratified random)
## 100 knots
## 0.1 units for radius of each case cluster
## within a unit square window (0,1),(0,1)
## case cluster centroids located at (0.25,0.75), (0.5,0.25), & (0.75,0.75)
## four simulation iterations (only control locations change between iteration)

## NOTE: Cannot easily restrict to a consistent sample size of control points

# Set seed for reproducibility
set.seed(1234)

# Simulate relative clustering
rand_pts4 <- rand_cascon(x_case = c(0.25, 0.5, 0.75),
                         y_case = c(0.75, 0.25, 0.75),
                         n_case = c(100, 100, 100),
                         n_control = 700,
                         r_case = c(0.1, 0.1, 0.1),
                         sim_total = 4,
                         samp_case = "uniform",
                         samp_control = "stratified",
                         n_knot = 10 ^ 2,
                         win = spatstat::unit.square()
                         )

lapply(rand_pts4, FUN = function(x) {x$n}) # double check sample size
lapply(rand_pts4, FUN = function(x) {table(x$marks)}) # double check prevalence

## Data Visualization
plot(rand_pts4, pch = 1, cex = c(0.5,0.1), cols = c("red", "blue"),
     main = "Stratified Random Simulation"
     )

# Example 5: Consistent random uniform disc of cases and inhomogenous Poisson process for controls

# Arguments 
## Same as in Example 2
## l_control: intensity function for controls

# Example
## Approximately 1000 total points
## 3 case clusters
## 100 case points within each case cluster (300 case points total)
## User-specified intensity function
## 0.1 units for radius of each case cluster
## within a unit square window (0,1),(0,1)
## case cluster centroids located at (0.25,0.75), (0.5,0.25), & (0.75,0.75)
## four simulation iterations (only control locations change between iteration)

## NOTE: Cannot easily restrict to a consistent sample size of control points

# Set seed for reproducibility
set.seed(1234)

# Lambda for the inhomogenous Poisson process of control locations
l_cont <- function(x, y) {1000 * exp(-3 * x) + 1000 * exp(-3 * y)}

# Simulate relative clustering
rand_pts5 <- rand_cascon(x_case = c(0.25, 0.5, 0.75),
                         y_case = c(0.75, 0.25, 0.75),
                         n_case = c(100, 100, 100),
                         #n_control = 700,
                         r_case = c(0.1, 0.1, 0.1),
                         sim_total = 4,
                         samp_case = "uniform",
                         samp_control = "IPP",
                         l_control = l_cont,
                         win = spatstat::unit.square()
                         )

lapply(rand_pts5, FUN = function(x) {x$n}) # double check sample size
lapply(rand_pts5, FUN = function(x) {table(x$marks)}) # double check prevalence

## Data Visualization
plot(rand_pts5, pch = 1, cex = c(0.5,0.1), cols = c("red", "blue"), 
     main = "IPP Sampling Simulation"
     )

# Example 6: Consistent random uniform disc of cases and random cluster sampling of controls (Neyman-Scott Process)

# Arguments 
## Same as in Example 2
## l_control: intensity function for controls
## e_control: expansion factor for controls
## n_cluster: sample size of controls per cluster
## r_control: radius of all control clusters
## same_n = T: logical to force a consistent sample size of control points

# Example
## Approximately 1000 total points
## 3 case clusters
## 100 case points within each case cluster (300 case points total)
## User-specified intensity function for parent points
## no expansion of the simulation window for generating parent points
## 0.1 units for radius of each case cluster
## 0.1 units for radius for each control cluster
## within a unit square window (0,1),(0,1)
## case cluster centroids located at (0.25,0.75), (0.5,0.25), & (0.75,0.75)
## four simulation iterations (only control locations change between iteration)

## NOTE: Can force a consistent sample size of control point by setting the argument 'same_n' to TRUE. Will increase computation time considerably. 

# Set seed for reproducibility
set.seed(1234)

# Kappa for the Neyman-Scott Process of control locations
l_cont <- function(x, y) {1000 * exp(-3 * x) + 1000 * exp(-3 * y)}

# Simulate relative clustering
rand_pts6 <- rand_cascon(x_case = c(0.25, 0.5, 0.75),
                         y_case = c(0.75, 0.25, 0.75),
                         n_case = c(100, 100, 100),
                         #n_control = 700,
                         r_case = c(0.1, 0.1, 0.1),
                         sim_total = 4,
                         samp_case = "uniform",
                         samp_control = "clustered",
                         #l_control = l_cont,
                         l_control = 67,
                         e_control = 0,
                         n_cluster = 13,
                         r_control = 0.1,
                         same_n = F,
                         win = spatstat::unit.square()
                         )

lapply(rand_pts6, FUN = function(x) {x$n}) # double check sample size
lapply(rand_pts6, FUN = function(x) {table(x$marks)}) # double check prevalence

## Data Visualization
plot(rand_pts6, pch = 1, cex = c(0.5,0.1), cols = c("red", "blue"),
     main = "Clustered Sampling Simulation"
     )

# Example 7: Consistent random homogeneous Poisson disc of cases and complete spatial randomness controls

# Arguments 
## Same as in Example 2
## l_case: lambda for Poisson case clustering

# Example
## Approximately 1000 total points
## 3 case clusters
## 700 control points within window (CSR sampling)
## case cluster 1 & 3 radii = 0.1 and case cluster 2 radius = 0.2
## case cluster 1 & 3 lambda = 300 and case cluster 2 radius = 500
## within a unit square window (0,1),(0,1)
## case cluster centroids located at (0.25,0.75), (0.5,0.25), & (0.75,0.75)
## four simulation iterations (only control locations change between iteration)

## NOTE 1: Cannot easily restrict to a consistent total sample size of case points
## NOTE 2: Cannot easily restrict to a consistent sample size of case points within each cluster

# Set seed for reproducibility
set.seed(1234)

# Simulate relative clustering
rand_pts7 <- rand_cascon(x_case = c(0.25, 0.5, 0.75),
                         y_case = c(0.75, 0.25, 0.75),
                         #n_case = c(100, 100, 100), # unnecessary for Poisson case clustering
                         n_control = 700,
                         r_case = c(0.1, 0.2, 0.1),
                         #l_case = 1000, # can do singular value for all clusters
                         l_case = c(300,500,300),
                         sim_total = 4,
                         samp_case = "Poisson",
                         samp_control = "CSR",
                         win = spatstat::unit.square()
                         )

lapply(rand_pts7, FUN = function(x) {x$n}) # double check sample size
lapply(rand_pts7, FUN = function(x) {table(x$marks)}) # double check prevalence

## Data Visualization
plot(rand_pts7, pch = 1, cex = c(0.5,0.1), cols = c("red", "blue"),
     main = "Poisson (CSR) Case Clusters"
     )

# Example 8: Consistent random inhomogeneous Poisson disc of cases and complete spatial randomness controls

# Arguments 
## Same as in Example 7
## Specify an intensity function for l_case for IPP

# Example
## Approximately 1000 total points
## 3 case clusters
## 700 control points within window (CSR sampling)
## case cluster 1 & 3 radii = 0.1 and case cluster 2 radius = 0.2
## case cluster 1 & 3 lambda = 300 and case cluster 2 radius = 500
## within a unit square window (0,1),(0,1)
## case cluster centroids located at (0.25,0.75), (0.5,0.25), & (0.75,0.75)
## four simulation iterations (only control locations change between iteration)

## NOTE 1: Cannot easily restrict to a consistent total sample size of case points
## NOTE 2: Cannot easily restrict to a consistent sample size of case points within each cluster

# Set seed for reproducibility
set.seed(1234)

# Lambda for the inhomogenous Poisson process of control locations
l_case1 <- function(x, y) {1000 * exp(-4 * x) + 1000 * exp(-4 * y)}
l_case2 <- function(x, y) {1000 * exp(-2 * x) + 1000 * exp(-2 * y)}
l_case3 <- function(x, y) {1000 * exp(-1 * x) + 1000 * exp(-1 * y)}

# Simulate relative clustering
rand_pts8 <-rand_cascon(x_case = c(0.25, 0.5, 0.75),
                        y_case = c(0.75, 0.25, 0.75),
                        #n_case = c(100, 100, 100), # unnecessary for Poisson case clustering
                        n_control = 700,
                        r_case = c(0.1, 0.2, 0.1),
                        #l_case = l_case3, # can do singular value for all clusters
                        l_case = c(l_case1, l_case2, l_case3),
                        sim_total = 4,
                        samp_case = "Poisson",
                        samp_control = "CSR",
                        win = spatstat::unit.square()
)

lapply(rand_pts8, FUN = function(x) {x$n}) # double check sample size
lapply(rand_pts8, FUN = function(x) {table(x$marks)}) # double check prevalence

## Data Visualization
plot(rand_pts8, pch = 1, cex = c(0.5,0.1), cols = c("red", "blue"),
     main = "IPP Case Clusters"
)
# -------------------- END OF CODE -------------------- #