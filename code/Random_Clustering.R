# ------------------------------------------ #
# Simulate and Combine Two Separate Marked Neyman-Scott Cluster Processes
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: April 7, 2020
#
# Recently modified by:
# Recently modified on:
#
# Notes:
# A) 4/7/20 (IB) - Potential simulation scheme for sparrpoweR package 0.0.0.9000
# B) 4/7/20 (IB) - Simulates a Neyman-Scott Cluster Process for cases and a separate Neyman-Scott Cluster Process for controls
# C) 4/7/20 (IB) - rNeymanScott helpfile: https://www.rdocumentation.org/packages/spatstat/versions/1.62-2/topics/rNeymanScott
# D) 4/7/20 (IB) - Allows functionality for user-specified prevalence 
# E) 4/7/20 (IB) - Allows functionality for user-specified, consistent sample sizes 
# F) 4/7/20 (IB) - To-do: 
#       1. Simulate dataset iteratively, spatstat has this functionalty within each function
# ------------------------------------------ #

############
# PACKAGES #
############

# install.packages("spatstat")
library(spatstat)

###################
# CUSTOM FUNCTION #
###################

random.casecon <- function(prevalence, n_total, n_case, n_control, k_case, k_control, e_case, e_control, r_case, r_control,...) {
  
  rcluster_case <- function(x0, y0, radius, n, types = "case") {
    X <- spatstat::runifdisc(n, radius, centre=c(x0, y0))
    spatstat::marks(X) <- types
    return(X)
  }
  
  rcluster_control <- function(x0, y0, radius, n, types = "control") {
    X <- spatstat::runifdisc(n, radius, centre=c(x0, y0))
    spatstat::marks(X) <- types
    return(X)
  }
  
  random.cluster <- function(n_cluster, ...) {
    repeat {
      # do something
      x <- spatstat::rNeymanScott(...)
      # exit if the condition is met
      if (x$n == n_cluster) break
    }
    return(x)
  }
  
  repeat {
    x <- random.cluster(kappa = k_case,
                        expand = e_case,
                        rcluster = rcluster_case, 
                        n = n_case,
                        radius = r_case,
                        n_cluster = prevalence*n_total
    )
    
    y <- random.cluster(kappa = k_control,
                        expand = e_control,
                        rcluster = rcluster_control, 
                        n = n_control,
                        radius = r_control,
                        n_cluster = (1-prevalence)*n_total
    )
    
    z <- spatstat::superimpose(x, y)
    
    return(z)
  }
}

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

rand_pts <- random.casecon(prevalence = 0.2, 
                           n_total = 1000,
                           n_case = 50,
                           n_control = 200,
                           k_case = 5, 
                           k_control = 5,
                           e_case = 0, 
                           e_control = 0, 
                           r_case = 0.1, 
                           r_control = 0.5,
                           win = spatstat::unit.square()
)

rand_pts$n # double check sample size
table(rand_pts$marks) # double check prevalence

## Data Visualization
plot(rand_pts, pch = 1, cols = c("red", "blue"), main = "Random Simulation")

# -------------------- END OF CODE -------------------- #
