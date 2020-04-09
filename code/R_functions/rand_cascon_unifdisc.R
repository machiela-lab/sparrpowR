# ------------------------------------------ #
# Function to Simulate and Combine User-Specified Case Clusters and Randomized Control Clusters
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: April 8, 2020
#
# Recently modified by: 
# Recently modified on:
#
# Notes:
# A) 4/8/20 (IB) - Potential simulation scheme for sparrpoweR package 0.0.0.9000
# B) 4/8/20 (IB) - Simulates a case cluster with user-specified parameters
# C) 4/8/20 (IB) - Maintains the case clusters as constant while simulating random control locations
# D) 4/8/20 (IB) - Initially provided functionality for complete spatial randomness of control locations
# E) 4/8/20 (IB) - Allows functionality for user-specified, consistent sample sizes
# ------------------------------------------ #

rand_cascon_unifdisc <- function(x_case, y_case, n_case, n_control, r_case, sim_total, ...) {
  
  require(spatstat)
  
  # marked uniform disc ppp with user-specified radius for cases
  rcluster_case <- function(x0, y0, radius, n, types = "case", ...) {
    repeat {  
      x <- spatstat::runifdisc(n, radius, centre=c(x0, y0), ...)
    if (x$n == n) break
    }
    spatstat::marks(x) <- types
    return(x)
  }
  
  # marked uniform ppp for controls
  rcluster_control <- function(n, types = "control", ...) {
    repeat {  
      x <- spatstat::rpoint(n, ...)
    if (x$n == n) break
    }
      spatstat::marks(x) <- types
    return(x)
  }
  
  pppList <- vector('list', length(sim_total)) # create empty list
  
  # Create a consistent random cluster of cases (uniform around user-specified centroid)
  x <- rcluster_case(x0 = x_case, y0 = y_case, radius = r_case, n = n_case, ...)
  
  # Simulate marked ppp for each iteration
  for(i in 1:sim_total) {
    
    # Create random cluster of controls
    y <- rcluster_control(n = n_control, ...)
    
    # Combine random clusters of cases and controls into one marked ppp
    z <- spatstat::superimpose(x, y)
    
    # Compile ppp into list
    pppList[[i]] <- z
    pppList[[i]]
  }
  class(pppList) <- c("ppplist", "solist",  "anylist", "listof", "list")
  
  return(pppList)
}

# -------------------- END OF CODE -------------------- #