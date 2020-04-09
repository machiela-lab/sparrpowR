# ------------------------------------------ #
# Function to Simulate and Combine User-Specified Case Clusters and Randomized Control Clusters
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: April 8, 2020
#
# Recently modified by: @idblr
# Recently modified on: April 9, 2020
#
# Notes:
# A) 4/8/20 (IB) - Potential simulation scheme for sparrpoweR package 0.0.0.9000
# B) 4/8/20 (IB) - Simulates case clusters with user-specified parameters
# C) 4/8/20 (IB) - Maintains the case clusters as constant while simulating random control locations
# D) 4/8/20 (IB) - Initially provided functionality for complete spatial randomness of control locations
# E) 4/8/20 (IB) - Allows functionality for user-specified, consistent sample sizes
# F) 4/8/20 (IB) - Added functionality for systematic control locations
# G) 4/8/20 (IB) - Added functionality for stratified random control locations. This is very similar to CSR, so likely not necessary
# H) 4/8/20 (IB) - Added functionality for inhomogenous Poisson control locations
# I) 4/9/20 (IB) - Added functionality for clustered control locations
# ------------------------------------------ #

rand_cascon_unifdisc <- function(x_case, y_case, n_case, n_control, n_cluster, r_case, r_control, sim_total, type_sampling = c("CSR", "systematic", "stratified", "IPP", "clustered"), n_knot, l_control, e_control, same_n = FALSE, ...) {
  
  # Packages
  require(spatstat)
  
  # Inputs
  if (length(x_case) != length(y_case)) {
    stop("There is at least one missing coordinate")
  }
  
  if (length(x_case) != length(r_case) | length(y_case) != length(r_case)) {
    stop("There is at least one radius (r_case) missing")
  }
  
  if (length(x_case) != length(n_case) | length(y_case) != length(n_case)) {
    stop("There is at least one case cluster sample size (n_case) missing")
  }
  
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
    if (type_sampling == "CSR") {
    repeat {  
      x <- spatstat::rpoispp(lambda = n, ...)
      if (x$n == n) break
      }
      }
      
    if (type_sampling == "systematic") {
      # repeat { 
      x <- spatstat::rsyst(nx = sqrt(n), ...)
      # if (x$n == n) break
      # }
    }
      
    if (type_sampling == "stratified") {
      # repeat {  
      x <- spatstat::rstrat(nx = round(sqrt(n_knot)), k = round(n/n_knot), ...)
      # if (x$n == n) break
      # }
    }
    
    if (type_sampling == "IPP") {
      if (class(l_control) != "function") {
        stop("The argument 'lambda' should be an intensity function")
      }
      # repeat {  
        x <- spatstat::rpoispp(lambda = l_control, ...)
       # if (x$n == n) break
       # }
    }
    
    if (type_sampling == "clustered") {
      
      rcluster_control <- function(x0, y0, radius, n) {
        X <- spatstat::runifdisc(n, radius, centre=c(x0, y0))
        return(X)
      }
      
      if(same_n == T){
       repeat {
      x <- spatstat::rNeymanScott(kappa = l_control,
                                  expand = e_control,
                                  rcluster = rcluster_control, 
                                  n = n_cluster,
                                  radius = r_control
                                  )
       if (x$n == n) break
       }
      } else {
        x <- spatstat::rNeymanScott(kappa = l_control,
                                    expand = e_control,
                                    rcluster = rcluster_control, 
                                    n = n_cluster,
                                    radius = r_control
        )
      }
    }
      spatstat::marks(x) <- types
    return(x)
  }
  
  # Create empty lists
  pppCase <- vector('list', length(x_case))
  pppList <- vector('list', length(sim_total))
  
  # Create a consistent random cluster of cases (uniform around user-specified centroid)
  for (i in 1:length(x_case)){
    x1 <- rcluster_case(x0 = x_case[i], y0 = y_case[i], radius = r_case[i], n = n_case[i], ...)
    pppCase[[i]] <- x1
  }
  class(pppCase) <- c("ppplist", "solist",  "anylist", "listof", "list")
  x <- spatstat::superimpose(pppCase)
  
  # Simulate marked ppp for each iteration
  for (j in 1:sim_total) {
    
    # Create random cluster of controls
    y <- rcluster_control(n = n_control, ...)
    
    # Combine random clusters of cases and controls into one marked ppp
    z <- spatstat::superimpose(x, y)
    
    # Compile ppp into list
    pppList[[j]] <- z
    pppList[[j]]
  }
  class(pppList) <- c("ppplist", "solist",  "anylist", "listof", "list")
  
  return(pppList)
}

# -------------------- END OF CODE -------------------- #