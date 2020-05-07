# ------------------------------------------ #
#' Function to Estimate the Power of a Spatial Relative Risk using previously 
#' collected data.
#'
#' There are other arguments for tuning (mostly for adaptive smoothing), see 
#' `sparr::risk()` helpfile.
#' 
#' * NOTE: Force the `sparr::risk()` arguement tolerate = TRUE to always calculate asymptotic p-vlaue surfaces
#' * NOTE: Force the `sparr::risk()` arguement verbose = FALSE to clean-up presentation
#' 
#' @param obs_data TO ADD
#' @param sim_total Number of simulation iterations
#' @param samp_control Type of random sampling for controls ('CSR', 'uniform', 'MVN')
#' @param s_control If MVN, the standard deviation of the random normal noise added to each coordinate of the control locations
#' @param upper_tail User-specified upper tail of a two-tailed significance level
#' @param lower_tail User-specified lower tail of a two-tailed significance level
#' @param cascon TRUE for power to detect both relative case and control clustering (hot and coldspots)
#' @param resolution 10 to calculate surfaces in a 10 x 10 grid
#' @param edge "diggle" to employ the Diggle method that reweights each observation-specific kernel, default is "uniform"
#' @param adapt FALSE to estimate using fixed smoothing, future direction: explore adaptive smoothing
#' @param h0 NULL for internal estimation of a common oversmoothing bandwidth computed via the `sparr::OS()` function in the sparr package, can be user specified if want to force same bandwidth across iterations
#' @param verbose TO ADD
#' @param parallel TO ADD
#' @param n_core TO ADD
#' @param ... TO ADD
#'
#' @return TO ADD, add output of the function here
#' @importFrom spatstat marks
#' @importFrom stats sd
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom foreach %do% %dopar%
#' @export
#'
#' @examples
#' ## From the 'sparr' package
#' \dontrun{
#' data(pbc)
#' sim_power <- jitter_power(obs_data = pbc,
#'                           sim_total = 100,
#'                           samp_control = "MVN",
#'                           parallel = TRUE,
#'                           s_control = 10, # default = 1
#'                           upper_tail = 0.995, # default = 0.975
#'                           lower_tail = 0.005, # default = 0.025
#'                           resolution = 100, # default = 128
#'                           edge = "diggle", # default = "uniform"
#'                           cascon = FALSE # default = FALSE
#'                          )
#' }
#' 
jitter_power <- function(obs_data,
                         sim_total,
                         samp_control = c("uniform", "CSR", "MVN"),
                         s_control = 1,
                         upper_tail = 0.975,
                         lower_tail = 0.025,
                         cascon = FALSE,
                         resolution = 128,
                         edge = "uniform",
                         adapt = FALSE,
                         h0 = NULL,
                         verbose = TRUE,
                         parallel = FALSE,
                         n_core = NULL,
                         ...) {
  
  # Custom Internal Functions
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
  rcluster_control <- function(n, l, win, s, types = "control", ...) {
    if (samp_control == "uniform") {
      repeat {  
        x <- spatstat::runifpoint(n = n, win = win, ...)
        if (x$n == n) break
      }
    }
    
    if (samp_control == "CSR") {
      x <- spatstat::rpoispp(lambda = l, win = win, ...)
    }
    
    if (samp_control == "MVN") {
        x1 <- obs_data$x + rnorm(length(obs_data$x), 0, s) 
        y1 <- obs_data$y + rnorm(length(obs_data$y), 0, s) 
        x <- spatstat::ppp(x1, y1, window = win)
    }
    
    spatstat::marks(x) <- types
    return(x)
  }
  
  # extract case locations
  cas <- obs_data[obs_data$marks == "case"]
  marks(cas) <- "case"
  
  if (verbose == TRUE & parallel == FALSE){
    message("Generating Data, Estimating Relative Risk, Calculating Power")
    pb <- txtProgressBar(min = 0, max = sim_total, style = 3)
  }
  
  ## Set function used in foreach
  if (parallel == TRUE){
    loadedPackages <- c("doParallel", "parallel")
    invisible(lapply(loadedPackages, require, character.only = T))
    if(is.null(n_core)){ n_core <- parallel::detectCores() - 1 }
    cl <- parallel::makeCluster(n_core)
    doParallel::registerDoParallel(cl)
    `%fun%` <- `%dopar%`
  } else {
    `%fun%` <- `%do%`
  }
  
  # Iteratively calculate the log relative risk and asymptotic p-value surfaces
  out_par <- foreach::foreach(k = 1:sim_total, 
                              .combine = comb, 
                              .multicombine = TRUE, 
                              .packages = c("sparr", "spatstat"),
                              .init = list(list(), list(), list(),
                                           list(), list(), list(), 
                                           list(), list(), list(), list()
                                           )
                              ) %fun% {
    
    # Progress bar
    if (verbose == TRUE & parallel == FALSE){
      setTxtProgressBar(pb, k)
      }
    
    # Create random cluster of controls
    con <- rcluster_control(n = obs_data[obs_data$marks == "control"]$n,
                          l = obs_data[obs_data$marks == "control"]$n / (diff(obs_data$window$xrange)*diff(obs_data$window$yrange)),
                          win = obs_data$window,
                          s = s_control,
                          ...)
    
    # Combine random clusters of cases and controls into one marked ppp
    z <- spatstat::superimpose(con, cas)
    spatstat::marks(z) <- as.factor(spatstat::marks(z))
    
    # Bandwidth selection
    if(is.null(h0)){
      h0 <- sparr::OS(z, nstar = "geometric")
    }
    
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
    sim_risk <- as.vector(t(obs_lrr$rr$v))
    sim_pval <- as.vector(t(obs_lrr$P$v))
    
    ### Estimated global test statistics
    #### Global maximum relative risk: H0 = 1
    s_obs <- max(exp(obs_lrr$rr$v[!is.na(obs_lrr$rr$v)]))
    #### Approximation for integral: H0 = 0
    t_obs <- sum((obs_lrr$rr$v[!is.na(obs_lrr$rr$v) & is.finite(obs_lrr$rr$v)]/(diff(obs_lrr$rr$xcol)[1]*diff(obs_lrr$rr$yrow)[1]))^2)
    
    ### Estimated value (log relative risk and p-value) for each knot
    if(k == 1) {
      sim <- z
      out <- obs_lrr
      } else {
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
                        "out" = out,
                        "n_con" = con$n,
                        "bandw" = h0,
                        "s_obs" = s_obs,
                        "t_obs" = t_obs
                        )
    return(par_results)
    }
  
  # Stop clusters, if parallel
  if(parallel == TRUE){
    parallel::stopCluster(cl)
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
                  "ry" = out_par[[4]][[1]],
                  "n_con" = unlist(out_par[[7]]),
                  "bandw" = unlist(out_par[[8]]),
                  "s_obs" = unlist(out_par[[9]]),
                  "t_obs" = unlist(out_par[[10]])
  )
}
# -------------------- END OF CODE -------------------- #