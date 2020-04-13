# ------------------------------------------ #
# Function to Estimate the Power of a Spatial Relative Risk using Simulated Data
# Version 2: Does all three steps for each iteration 
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: April 12, 2020
#
# Recently modified by:
# Recently modified on:
#
# Notes:
# A) 4/13/20 (IB) - Combines rand_cascon() and rand_srr() functions per iteration
# ------------------------------------------ #

sparrpowR <- function(x_case, y_case,
                      n_case = NULL, n_control, n_cluster, n_knot,
                      r_case, r_control,
                      l_control, l_case = NULL,
                      e_control,
                      sim_total,
                      samp_case = c("uniform", "Poisson"),
                      samp_control = c("CSR", "systematic", "stratified",
                                       "IPP", "clustered"),
                      same_n = FALSE, 
                      upper_tail = 0.975,
                      lower_tail = 0.025,
                      parallel = FALSE, 
                      win = unit.square(), ...) {
  
  # Packages
  require(spatstat)
  require(sparr)
  require(foreach)
  require(svMisc)
  
  # Custom Internal Functions
  ## Set function used in foreach
  if (parallel == TRUE){
    `%fun%` <- `%dopar%`
  } else {
    `%fun%` <- `%do%`
  }
  
  ## Combine function used in foreach
  comb <- function(x, ...) {
    lapply(seq_along(x),
           function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
  }
  
  ## Calculate proportion of runs as significant
  proportionSignificant <- function(x) {
    x / sim_total
  }
  
  # Inputs
  if (length(x_case) != length(y_case)) {
    stop("There is at least one missing coordinate")
  }
  
  if (length(x_case) != length(r_case) | length(y_case) != length(r_case)) {
    stop("There is at least one radius (r_case) missing")
  }
  
  if (samp_case == "Poisson" & length(l_case) == 1) {
    l_list <- vector('list', length(x_case))
    for (l in 1:length(x_case)) {
      l_list[[l]] <- l_case
    }
  }
  
  # marked uniform disc ppp with user-specified radius for cases
  rcluster_case <- function(x0, y0, radius, n = NULL, types = "case", lambda = NULL, win = NULL, ...) {
    
    if (samp_case == "uniform"){
      if (length(x_case) != length(n_case)) {
        stop("There is at least one case cluster sample size (n_case) missing")
      }
      
      repeat {  
        x <- spatstat::runifdisc(n, radius, centre = c(x0, y0), ...)
        if (x$n == n) break
      }
    }  
    
    if (samp_case == "Poisson"){
      win_case <- spatstat::disc(radius, centre = c(0.5, 0.5), ...)
      # repeat { 
      x <- spatstat::rpoispp(lambda, win = win_case, ...)
      x <- spatstat::shift(x, c(x0 - 0.5, y0 - 0.5))
      # if (x$n == n) break
      # }
    }
    spatstat::marks(x) <- types
    return(x)
  }
  
  # marked uniform ppp for controls
  rcluster_control <- function(n, types = "control", ...) {
    if (samp_control == "CSR") {
      repeat {  
        x <- spatstat::rpoispp(lambda = n, ...)
        if (x$n == n) break
      }
    }
    
    if (samp_control == "systematic") {
      # repeat { 
      x <- spatstat::rsyst(nx = sqrt(n), ...)
      # if (x$n == n) break
      # }
    }
    
    if (samp_control == "stratified") {
      # repeat {
      x <- spatstat::rstrat(nx = round(sqrt(n_knot)), k = round(n/n_knot), ...)
      # if (x$n == n) break
      # }
    }
    
    if (samp_control == "IPP") {
      if (class(l_control) != "function") {
        stop("The argument 'lambda' should be an intensity function")
      }
      # repeat {  
      x <- spatstat::rpoispp(lambda = l_control, ...)
      # if (x$n == n) break
      # }
    }
    
    if (samp_control == "clustered") {
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
  
  # Create a consistent random cluster of cases (uniform around user-specified centroid)
  for (i in 1:length(x_case)){
    if(samp_case == "uniform"){
      x1 <- rcluster_case(x0 = x_case[i], y0 = y_case[i],
                          radius = r_case[i], n = n_case[i],
                          lambda = NULL, ...)
    }
    
    if(samp_case == "Poisson"){
      if (length(l_case) == 1) {
        x1 <- rcluster_case(x0 = x_case[i], y0 = y_case[i],
                            radius = r_case[i], n = NULL,
                            lambda = l_list[[i]], ...)
      } else {
        x1 <- rcluster_case(x0 = x_case[i], y0 = y_case[i],
                            radius = r_case[i], n = NULL,
                            lambda = l_case[[i]], ...)
      }
    }
    pppCase[[i]] <- x1
  }
  class(pppCase) <- c("ppplist", "solist",  "anylist", "listof", "list")
  x <- spatstat::superimpose(pppCase)
  
  # Progress bar
  message("Generating Data, Estimating Relative Risk, Calculating Power")
  pb <- txtProgressBar(min = 0, max = sim_total, style = 3)
  
  # Iteratively calculate the log relative risk and asymptotic p-value surfaces
  out_par <- foreach::foreach(k = 1:sim_total, 
                              .combine = comb, 
                              .multicombine = TRUE, 
                              .packages = c("sparr", "spatstat"),
                              .init = list(list(), list(), list(), list(), list(), list())
  ) %fun% {
    
    # Progress bar
    setTxtProgressBar(pb, k)
    
    # Create random cluster of controls
    y <- rcluster_control(n = n_control, ...)
    
    # Combine random clusters of cases and controls into one marked ppp
    z <- spatstat::superimpose(x, y)
    
    # Calculate observed kernel density ratio
    obs_lrr <- sparr::risk(z, tolerate = T, ...)
    
    # Output processing for visualization and summary across iterations
    ## Convert output matrix to two output vectors
    ### Coordinates for each knot
    rx <- rep(obs_lrr$rr$xcol, length(obs_lrr$rr$yrow))
    for(i in 1:length(obs_lrr$rr$yrow)){
      if (i == 1){ ry <- rep(obs_lrr$rr$yrow[i], length(obs_lrr$rr$xcol))}
      if (i != 1){ ry <- c(ry, rep(obs_lrr$rr$yrow[i], length(obs_lrr$rr$xcol)))}
    }
    ### Estimated value (log relative risk and p-value) for each knot
    if(k == 1) {
      sim_risk = as.vector(t(obs_lrr$rr$v))
      sim_pval = as.vector(t(obs_lrr$P$v))
      sim <- z
      out <- obs_lrr
    } else {
      sim_risk <- as.vector(t(obs_lrr$rr$v))
      sim_pval <- as.vector(t(obs_lrr$P$v))
      sim <- NULL
      out <- NULL
      rx <- NULL
      ry <- NULL
    }
    
    # Output for each n-fold
    par_results <- list("sim_risk" = sim_risk,
                        "sim_pval" = sim_pval,
                        "rx" = rx,
                        "ry" = ry,
                        "sim" = sim,
                        "out" = out
    )
    
    return(par_results)
  }
  
  # Summarize iterative results
  sim_rr <- out_par[[1]]
  sim_pval <- out_par[[2]]
  
  ## Set -Inf and Inf to NA values in order to capture in output
  sim_rr_dat <- do.call(cbind, sim_rr)
  sim_rr_dat <- ifelse(sim_rr_dat == -Inf, NA, sim_rr_dat)
  sim_rr_dat <- ifelse(sim_rr_dat == Inf, NA, sim_rr_dat)
  
  ## Calculate Mean and Standard Deviation
  rr_mean <- rowMeans(sim_rr_dat, na.rm = TRUE) # mean log relative risk
  pval_mean <-  rowMeans(do.call(cbind, sim_pval), na.rm = TRUE) # mean p-value
  rr_sd <- apply(sim_rr_dat, 1, sd, na.rm = TRUE) # standard deviation log relative risk
  
  ## Calculate proportion of tests were significant
  ### Significance level is user-specified
  pval_sig <- rapply(sim_pval, function(x) ifelse(x < lower_tail | x > upper_tail , TRUE, FALSE), how = "replace")
  pval_count <- rowSums(do.call(cbind,pval_sig), na.rm = TRUE)
  pval_prop_wNA <- sapply(pval_count, FUN = proportionSignificant)
  
  ## Force NA values for graphing, match position of NAs of mean p-value
  pval_prop_wNA <- cbind(pval_mean,pval_prop_wNA)
  pval_prop_wNA[,2][is.na(pval_prop_wNA[,1])] <- NA
  pval_prop <- pval_prop_wNA[,2]
  
  # Output
  out_sim <- list("sim" = out_par[[5]][[1]],
                  "out" = out_par[[6]][[1]],
                  "rr_mean" = rr_mean,
                  "pval_mean" = pval_mean,
                  "rr_sd" = rr_sd,
                  "pval_prop" = pval_prop,
                  "rx" = out_par[[3]][[1]],
                  "ry" = out_par[[4]][[1]]
  )
}
# -------------------- END OF CODE -------------------- #