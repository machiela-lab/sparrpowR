# ------------------------------------------ #
# Function to Simulate and Combine Two Separate Marked Neyman-Scott Cluster Processes
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
# ------------------------------------------ #

rand_cascon_neymscot <- function(prevalence, n_total, n_case, n_control, k_case, k_control, e_case, e_control, r_case, r_control, sim_total, ...) {
  
  require(spatstat)
  
  # marked Neyman-Scott process with a consistent sample size
  random.cluster <- function(n_cluster, ...) {
    repeat {
      x <- spatstat::rNeymanScott(...)
      if (x$n == n_cluster) break
    }
    return(x)
  }
  
  # marked daughter ppp for cases
  rcluster_case <- function(x0, y0, radius, n, types = "case") {
    X <- spatstat::runifdisc(n, radius, centre=c(x0, y0))
    spatstat::marks(X) <- types
    return(X)
  }
  
  # marked daughter ppp for controls
  rcluster_control <- function(x0, y0, radius, n, types = "control") {
    X <- spatstat::runifdisc(n, radius, centre=c(x0, y0))
    spatstat::marks(X) <- types
    return(X)
  }
  
  pppList <- vector('list', length(sim_total)) # create empty list
  
  # Simulate marked ppp for each iteration
  for(i in 1:sim_total) {
    
    # Create random cluster of cases
    x <- random.cluster(kappa = k_case,
                        expand = e_case,
                        rcluster = rcluster_case, 
                        n = n_case,
                        radius = r_case,
                        n_cluster = prevalence*n_total,
                        ...
    )
    
    # Create random cluster of controls
    y <- random.cluster(kappa = k_control,
                        expand = e_control,
                        rcluster = rcluster_control, 
                        n = n_control,
                        radius = r_control,
                        n_cluster = (1-prevalence)*n_total,
                        ...
    )
    
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