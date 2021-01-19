#' Power of SRR function for previously collected data.
#'
#' Compute the statistical power of a spatial relative risk function using previously collected data.
#' 
#' @param obs_data A bivariate point pattern (a multitype point pattern of object of class "ppp") with two types of points in a factor valued mark.
#' @param sim_total Integer, specifying the number of simulation iterations to perform.
#' @param samp_control Character string specifying whether to randomize the control locations uniformly (\code{samp_control="uniform"}), with complete spatial randomness (\code{samp_control="CSR"}), or multivariate normal (\code{samp_control="MVN"}).
#' @param s_control Optional. Numeric value for the standard deviation of the multivariate normal distribution in the units of the \code{obs_data}.  The default value (1) assumes a unit square window. Ignored if Ignored if \code{samp_control="uniform"} or \code{samp_control="CSR"}.
#' @param lower_tail Optional. Numeric value of lower p-value threshold (default=0.025).
#' @param upper_tail Optional. Numeric value of upper p-value threshold (default=0.975).
#' @param parallel Logical. If TRUE, will execute the function in parallel. If FALSE (the default), will not execute the function in parallel.
#' @param n_core Optional. Integer specifying the number of CPU cores on current host to use for parallelization (the default is 2 cores).
#' @param verbose Logical. If TRUE (the default), will print function progress during execution. If FALSE, will not print.
#' @param ... Arguments passed to \code{\link[sparr]{risk}} to select bandwidth, edge correction, and resolution.
#' 
#' @details This function computes the statistical power of the spatial relative risk function (nonparametric estimate of relative risk by kernel smoothing) for previously collected studies with known case and control locations. 
#' 
#' The function uses the \code{\link[sparr]{risk}} function to estimate the spatial relative risk function and forces the \code{tolerate} argument to be TRUE in order to calculate asymptotic p-values.
#' 
#' If \code{samp_control = "uniform"} the control locations are randomly generated uniformly within the window of \code{obs_data}. By default, the resolution is an integer value of 128 and can be specified using the \code{resolution} argument in the internally called \code{\link[sparr]{risk}} function.
#' 
#' If \code{samp_control = "CSR"} the control locations are randomly generated assuming complete spatial randomness (homogeneous Poisson process) within the window of \code{obs_data} with a \code{lambda = number of controls / [resolution x resolution]}. By default, the resolution is an integer value of 128 and can be specified using the \code{resolution} argument in the internally called \code{\link[sparr]{risk}} function.
#' 
#' If \code{samp_control = "MVN"} the control locations are randomly generated assuming a multivariate normal distribution \emph{centered at each observed location}. The optional argument \code{s_control} specifies the standard deviation of the multivariate normal distribution (1 by default) in the units of the \code{obs_data}. 
#'
#' @return An object of class "list". This is a named list with the following components:
#' 
#' \describe{
#' \item{\code{sim}}{An object of class 'rrs' for the first iteration of simulated data.}
#' \item{\code{out}}{An object of class 'rrs' for the observed spatial relative risk function without randomization.}
#' \item{\code{rr_mean}}{Vector of length \code{[resolution x resolution]} of the mean relative risk values at each gridded knot.}
#' \item{\code{pval_mean}}{Vector of length \code{[resolution x resolution]} of the mean asymptotic p-value at each gridded knot.}
#' \item{\code{rr_sd}}{Vector of length \code{[resolution x resolution]} of the standard deviation of relative risk values at each gridded knot.}
#' \item{\code{pval_prop_cascon}}{Vector of length \code{[resolution x resolution]} of the proportion of asymptotic p-values that were significant for both case and control locations at each gridded knot.}
#' \item{\code{pval_prop_cas}}{Vector of length \code{[resolution x resolution]} of the proportion of asymptotic p-values that were significant for only case locations at each gridded knot.}
#' \item{\code{rx}}{Vector of length \code{[resolution x resolution]} of the x-coordinates of each gridded knot.}
#' \item{\code{ry}}{Vector of length \code{[resolution x resolution]} of the y-coordinates of each gridded knot.}
#' \item{\code{n_cas}}{Vector of length \code{sim_total} of the number of case locations simulated in each iteration.}
#' \item{\code{n_con}}{Vector of length \code{sim_total} of the number of control locations simulated in each iteration.}
#' \item{\code{bandw}}{Vector of length \code{sim_total} of the bandwidth (of numerator) used in each iteration.}
#' \item{\code{s_obs}}{Vector of length \code{sim_total} of the global s statistic.}
#' \item{\code{t_obs}}{Vector of length \code{sim_total} of the global t statistic.}
#' }
#' 
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach %do% %dopar% foreach
#' @importFrom parallel makeCluster stopCluster
#' @importFrom sparr risk
#' @importFrom spatstat.core rpoispp runifpoint
#' @importFrom spatstat.geom marks ppp superimpose
#' @importFrom stats sd
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
#' 
#' @seealso \code{\link[sparr]{risk}} for additional arguments for bandwidth selection, edge correction, and resolution.
#'
#' @examples
#' # Using the \code{\link[spatstat.data]{chorley}} dataset
#' data(chorley)
#'  f1 <- jitter_power(obs_data = unique(chorley),
#'                     sim_total = 2,
#'                     samp_control = "MVN",
#'                     s_control = 0.01,
#'                     verbose = FALSE)
#' 
jitter_power <- function(obs_data,
                         sim_total,
                         samp_control = c("uniform", "CSR", "MVN"),
                         s_control = 1,
                         lower_tail = 0.025, 
                         upper_tail = 0.975,
                         parallel = FALSE,
                         n_core = 2,
                         verbose = TRUE,
                         ...) {
  
  # Input
  if (class(obs_data) != "ppp"){
    stop("Argument 'obs_data' must be of class 'ppp'")
  }
  
  # marked uniform ppp for controls
  rcluster_control <- function(n, l, win, s, types = "control", ...) {
    if (samp_control == "uniform") {
      repeat {  
        x <- spatstat.core::runifpoint(n = n, win = win, ...)
        if (x$n == n) break
      }
    }
    
    if (samp_control == "CSR") {
      x <- spatstat.core::rpoispp(lambda = l, win = win, ...)
    }
    
    if (samp_control == "MVN") {
      x1 <- obs_data$x + rnorm(length(obs_data$x), 0, s) 
      y1 <- obs_data$y + rnorm(length(obs_data$y), 0, s) 
      x <- spatstat.geom::ppp(x1, y1, window = win)
    }
    
    spatstat.geom::marks(x) <- types
    return(x)
  }
  
  # extract case locations
  cas <- split(obs_data)[[1]]
  spatstat.geom::marks(cas) <- "case"
  
  # progress bar
  if (verbose == TRUE & parallel == FALSE){
    message("Generating Data, Estimating Relative Risk, Calculating Power")
    pb <- utils::txtProgressBar(min = 0, max = sim_total, style = 3)
  }
  
  ## Set function used in foreach
  if (parallel == TRUE){
    cl <- parallel::makeCluster(n_core)
    doParallel::registerDoParallel(cl)
    `%fun%` <- foreach::`%dopar%`
  } else { `%fun%` <- foreach::`%do%` }
  
  # Iteratively calculate the log relative risk and asymptotic p-value surfaces
  out_par <- foreach::foreach(k = 1:sim_total, 
                              .combine = comb, 
                              .multicombine = TRUE, 
                              .packages = c("sparr", "spatstat.core", "utils"),
                              .init = list(list(), list(), list(),
                                           list(), list(), list(), 
                                           list(), list(), list(),
                                           list(), list())) %fun% {
    
    # Progress bar
    if (verbose == TRUE & parallel == FALSE){
      utils::setTxtProgressBar(pb, k)
      if (k == sim_total) cat("\n")
    }
    
    # Create random cluster of controls
    con <- rcluster_control(n = split(obs_data)[[2]]$n,
                            l = split(obs_data)[[2]]$n
                                / (diff(obs_data$window$xrange) 
                                * diff(obs_data$window$yrange)),
                            win = obs_data$window,
                            s = s_control,
                            ...)
    
    # Combine random clusters of cases and controls into one marked ppp
    z <- spatstat.geom::superimpose(con, cas)
    spatstat.geom::marks(z) <- as.factor(spatstat.geom::marks(z))
    
    # Calculate observed kernel density ratio
    obs_lrr <- sparr::risk(z, tolerate = TRUE, verbose = FALSE, ...)
    
    # Output processing for visualization and summary across iterations
    ## Convert output matrix to two output vectors
    ### Coordinates for each knot
    rx <- rep(obs_lrr$rr$xcol, length(obs_lrr$rr$yrow))
    for(i in 1:length(obs_lrr$rr$yrow)){
      if (i == 1) { ry <- rep(obs_lrr$rr$yrow[i], length(obs_lrr$rr$xcol)) }
      if (i != 1) { ry <- c(ry, rep(obs_lrr$rr$yrow[i], length(obs_lrr$rr$xcol))) }
    }
    
    ### Estimated value (log relative risk and p-value) for each knot
    sim_risk <- as.vector(t(obs_lrr$rr$v))
    sim_pval <- as.vector(t(obs_lrr$P$v))
    
    ### Estimated global test statistics
    #### Global maximum relative risk: H0 = 1
    s_obs <- max(exp(obs_lrr$rr$v[!is.na(obs_lrr$rr$v)]))
    #### Approximation for integral: H0 = 0
    t_obs <- sum((obs_lrr$rr$v[!is.na(obs_lrr$rr$v) 
                               & is.finite(obs_lrr$rr$v)] / (diff(obs_lrr$rr$xcol)[1] 
                                                             * diff(obs_lrr$rr$yrow)[1]))^2)
    
    ### Estimated value (log relative risk and p-value) for each knot
    if (k == 1) {
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
                        "n_cas" = cas$n,
                        "n_con" = con$n,
                        "bandw" = obs_lrr$f$h0,
                        "s_obs" = s_obs,
                        "t_obs" = t_obs)
    return(par_results)
  }
  
  # Stop clusters, if parallel
  if (parallel == TRUE){
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
  #### Case and Control (lower and upper tail)
  pval_sig_cascon <- rapply(sim_pval, function(x) ifelse(x < lower_tail | x > upper_tail,
                                                         TRUE,
                                                         FALSE),
                            how = "replace")
  pval_count_cascon <- rowSums(do.call(cbind,pval_sig_cascon), na.rm = TRUE)
  pval_prop_wNA_cascon <- sapply(pval_count_cascon, FUN = function(x, y = sim_total) (x / y))
  #### Case only (lower tail only)
  pval_sig_cas <- rapply(sim_pval, function(x) ifelse(x < lower_tail, TRUE, FALSE),
                         how = "replace")
  pval_count_cas <- rowSums(do.call(cbind,pval_sig_cas), na.rm = TRUE)
  pval_prop_wNA_cas <- sapply(pval_count_cas, FUN = function(x, y = sim_total) (x / y))
  
  ## Force NA values for graphing, match position of NAs of mean p-value
  #### Case and Control (lower and upper tail)
  pval_prop_wNA_cascon <- cbind(pval_mean,pval_prop_wNA_cascon)
  pval_prop_wNA_cascon[,2][is.na(pval_prop_wNA_cascon[,1])] <- NA
  pval_prop_cascon <- pval_prop_wNA_cascon[,2]
  #### Case only (lower tail only)
  pval_prop_wNA_cas <- cbind(pval_mean,pval_prop_wNA_cas)
  pval_prop_wNA_cas[,2][is.na(pval_prop_wNA_cas[,1])] <- NA
  pval_prop_cas <- pval_prop_wNA_cas[,2]
  
  # Output
  out_sim <- list("sim" = out_par[[5]][[1]],
                  "out" = out_par[[6]][[1]],
                  "rr_mean" = rr_mean,
                  "pval_mean" = pval_mean,
                  "rr_sd" = rr_sd,
                  "pval_prop_cascon" = pval_prop_cascon,
                  "pval_prop_cas" = pval_prop_cas,
                  "rx" = out_par[[3]][[1]],
                  "ry" = out_par[[4]][[1]],
                  "n_cas" = unlist(out_par[[7]]),
                  "n_con" = unlist(out_par[[8]]),
                  "bandw" = unlist(out_par[[9]]),
                  "s_obs" = unlist(out_par[[10]]),
                  "t_obs" = unlist(out_par[[11]]))
}
