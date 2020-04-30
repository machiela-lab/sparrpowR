# ------------------------------------------ #
# Function to Iteratively Estimate the Spatial Relative Risk for Simulated Data
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: April 10, 2020
#
# Recently modified by:
# Recently modified on:
#
# Notes:
# A) 4/10/20 (IB) - Modified from @idblr dissertation 
# ------------------------------------------ #

# Data structure
# sim_locs = marked planar point pattern with a binary factor mark

rand_srr <- function(sim_locs, upper_tail = 0.975, lower_tail = 0.025, parallel = FALSE, ...) {
  
  # Packages
  require("sparr")
  require("foreach")
  
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
    x / length(sim_locs)
  }
  
  # Iteratively calculate the log relative risk and asymptotic p-value surfaces
  out_par <- foreach::foreach(k = 1:length(sim_locs), 
                              .combine = comb, 
                              .multicombine = TRUE, 
                              .packages = "sparr",
                              .init = list(list(), list(), list(), list())
                              ) %fun% {
  
  # Calculate observed kernel density ratio
  obs_lrr <- sparr::risk(sim_locs[[k]], tolerate = T, ...)
  
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
  } else {
    rx <- NULL
    ry <- NULL
    sim_risk = as.vector(t(obs_lrr$rr$v))
    sim_pval = as.vector(t(obs_lrr$P$v))
  }
  
  # Output for each n-fold
  par_results <- list("sim_risk" = sim_risk,
                      "sim_pval" = sim_pval,
                      "rx" = rx,
                      "ry" = ry
                      )

  return(par_results)
  }

# Summarize iterative results
sim_rr <- out_par[[1]]
sim_pval <- out_par[[2]]
rx <- out_par[[3]][[1]]
ry <- out_par[[4]][[1]]

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
out_sim <- list("rr_mean" = rr_mean,
                "pval_mean" = pval_mean,
                "rr_sd" = rr_sd,
                "pval_prop" = pval_prop,
                "rx" = rx,
                "ry" = ry
                )
}
# -------------------- END OF CODE -------------------- #