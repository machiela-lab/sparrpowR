# ------------------------------------------ #
# Function to Estimate the Power of a Spatial Relative Risk using previously collected data
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

jitter_power <- function(obs_data,
                         sim_total,
                         samp_control = c("CSR", "uniform", "jitter"),
                         scalar = 1,
                         upper_tail = 0.975,
                         lower_tail = 0.025,
                         parallel = FALSE,
                         cascon = FALSE,
                         ...) {
  
  # Packages
  require(spatstat)
  require(sparr)
  require(foreach)
  
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
  
  # Input
  if(class(obs_data) != "ppp"){
    stop("Argument 'obs_data' must be of class 'ppp'")
  }
  
  # marked uniform ppp for controls
  rcluster_control <- function(n, l, win, types = "control", ...) {
    if (samp_control == "CSR") {
        x <- spatstat::rpoispp(lambda = l, win = win, ...)
    }
    
    if (samp_control == "uniform") {
      repeat {  
        x <- spatstat::runifpoint(n = n, win = win, ...)
        if (x$n == n) break
      }
    }
    
    if (samp_control == "jitter") {
        x <- cbind(obs_data$x,obs_data$y)
        x <-  x + rnorm(length(obs_data), 0, scalar) 
        x <- spatstat::ppp(x[,1], x[,2], window = win)
    }
    
    spatstat::marks(x) <- types
    return(x)
  }
  
  # extract case locations
  x <- obs_data[obs_data$marks == "case"]
  marks(x) <- "case"
  
  # Progress bar
  message("Generating Data, Estimating Relative Risk, Calculating Power")
  pb <- txtProgressBar(min = 0, max = sim_total, style = 3)
  
  # Iteratively calculate the log relative risk and asymptotic p-value surfaces
  out_par <- foreach::foreach(k = 1:sim_total, 
                              .combine = comb, 
                              .multicombine = TRUE, 
                              .packages = c("sparr", "spatstat"),
                              .init = list(list(), list(), list(),
                                           list(), list(), list()
                                           )
                              ) %fun% {
    
    # Progress bar
    setTxtProgressBar(pb, k)
    
    # Create random cluster of controls
    y <- rcluster_control(n = obs_data[obs_data$marks == "control"]$n,
                          l = obs_data[obs_data$marks == "control"]$n / (diff(obs_data$window$xrange)*diff(obs_data$window$yrange)),
                          win = obs_data$window,
                          s = scalar,
                          ...)
    
    # Combine random clusters of cases and controls into one marked ppp
    z <- spatstat::superimpose(x, y)
    
    # Calculate observed kernel density ratio
    obs_lrr <- sparr::risk(z, tolerate = T, verbose = F, ...)
    
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
  if(cascon == TRUE){
    pval_sig <- rapply(sim_pval, function(x) ifelse(x < lower_tail | x > upper_tail , TRUE, FALSE), how = "replace")
  } else {
    pval_sig <- rapply(sim_pval, function(x) ifelse(x < lower_tail, TRUE, FALSE), how = "replace")
  }
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