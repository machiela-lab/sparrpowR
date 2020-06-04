#' Power of SRR function for randomly generated data.
#' 
#' Compute the statistical power of a spatial relative risk function using randomly generated data.
#'
#' @param win Window in which to simulate the random data. An object of class "owin" or something acceptable to \code{\link[spatstat]{as.owin}}.
#' @param sim_total Integer, specifying the number of simulation iterations to perform.
#' @param x_case Numeric value, or numeric vector, of x-coordinate(s) of case cluster(s).
#' @param y_case Numeric value, or numeric vector, of y-coordinate(s) of case cluster(s).
#' @param samp_case Character string specifying whether to randomize the case locations uniformly (\code{samp_control="uniform"}), multivariate normal (\code{samp_control="MVN"}), with complete spatial randomness (\code{samp_control="CSR"}), or using the inhomogeneous Poisson process (\code{samp_control="IPP"}) around each case centroid.
#' @param samp_control Character string specifying whether to randomize the control locations uniformly (\code{samp_control="uniform"}), systematically (\code{samp_control="systematic"}), multivariate normal (\code{samp_control="MVN"}), with complete spatial randomness (\code{samp_control="CSR"}), using the inhomogeneous Poisson process (\code{samp_control="IPP"}), or a realisation of the Neyman-Scott cluster process (\code{samp_control="clustered"}).
#' @param n_case Numeric value, or numeric vector, of the sample size for case locations in each cluster.
#' @param n_control Numeric value, or numeric vector, of the sample size for control locations in each cluster.
#' @param npc_control Optional. Numeric value of the number of clusters of control locations. Ignored if \code{samp_control!="clustered"}.
#' @param x_control Numeric value, or numeric vector, of x-coordinate(s) of case cluster(s). Ignored if \code{samp_control!="MVN"}.
#' @param y_control Numeric value, or numeric vector, of y-coordinate(s) of case cluster(s). Ignored if \code{samp_control!="MVN"}.
#' @param r_case Optional. Numeric value, or numeric vector, of radius (radii) of case cluster(s) in the units of \code{win}. Ignored if \code{samp_case="MVN"}.
#' @param r_control Optional. Numeric value, or numeric vector, of radius (radii) of control cluster(s) in the units of \code{win}. Ignored if \code{samp_control!="clustered"}.
#' @param s_case Optional. Numeric value, or numeric vector, for the standard deviation(s) of the multivariate normal distribution for case locations in the units of \code{win}. Ignored if \code{samp_control!="MVN"}.
#' @param s_control Optional. Numeric value, or numeric vector, for the standard deviation(s) of the multivariate normal distribution for control locations in the units of \code{win}. Ignored if \code{samp_control!="MVN"}.
#' @param l_case Optional. A single positive number, a vector of positive numbers, a function(x,y, ...), or a pixel image. Intensity of the Poisson process for case clusters. Ignored if \code{samp_control!="IPP"}.
#' @param l_control Optional. A single positive number, a vector of positive numbers, a function(x,y, ...), or a pixel image. Intensity of the Poisson process for control clusters. Ignored if \code{samp_control="uniform"}, \code{samp_control="systematic"}, \code{samp_control="MVN"}, or \code{samp_control="CSR"}.
#' @param e_control Optional. A single non-negative number for the size of the expansion of the simulation window for generating parent points. Ignored if \code{samp_control!="clustered"}.
#' @param cascon Logical. If TRUE, computes the statistical power to detect case clusters and control clusters. If FALSE (the default), computes the statistical power to detect case clusters only. 
#' @param lower_tail Optional. Numeric value of lower p-value threshold (default=0.025).
#' @param upper_tail Optional. Numeric value of upper p-value threshold (default=0.975). Ignored if cascon=FALSE.
#' @param parallel Logical. If TRUE, will execute the function in parallel. If FALSE (the default), will not execute the function in parallel.
#' @param n_core Optional. Integer specifying the number of CPU cores on current host to use for parallelization (the default is 2 cores).
#' @param verbose Logical. If TRUE (the default), will print function progress during execution. If FALSE, will not print.
#' @param ... Arguments passed to \code{\link[spatstat]{runifdisc}}, \code{\link[spatstat]{disc}}, \code{\link[spatstat]{rpoispp}}, \code{\link[spatstat]{rsyst}}, or \code{\link[spatstat]{rNeymanScott}} depending on \code{samp_control} or \code{samp_control}. Arguments also passed to \code{\link[sparr]{risk}} to select bandwidth, edge correction, and resolution.
#'
#' @details This function computes the statistical power of the spatial relative risk function (nonparametric estimate of relative risk by kernel smoothing) for randomly generated data using various random point pattern generators from the \code{\link{spatstat}} package.
#' 
#' The function uses the \code{\link[sparr]{risk}} function to estimate the spatial relative risk function and forces the \code{tolerate} argument to be TRUE in order to calculate asymptotic p-values.
#' 
#' If \code{samp_case = "uniform"} the case locations are randomly generated uniformly within a disc of radius \code{r_case} (or discs of radii \code{r_case}) centered at coordinates (\code{x_case}, \code{y_case}). 
#' 
#' If \code{samp_case = "MVN"} the case locations are randomly generated assuming a multivariate normal distribution centered at coordinates (\code{x_case}, \code{y_case}) with a standard deviation of \code{s_case}.
#' 
#' If \code{samp_case = "CSR"} the case locations are randomly generated assuming complete spatial randomness (homogeneous Poisson process) within a disc of radius \code{r_case} (or discs of radii \code{r_case}) centered at coordinates (\code{x_case}, \code{y_case}) with \code{lambda = n_case / area of disc}.
#' 
#' If \code{samp_case = "IPP"} the case locations are randomly generated assuming an inhomogeneous Poisson process with a disc of radius \code{r_case} (or discs of radii \code{r_case}) centered at coordinates (\code{x_case}, \code{y_case}) with \code{lambda = l_case}, a function.
#' 
#' If \code{samp_control = "uniform"} the control locations are randomly generated uniformly within the window \code{win}.
#' 
#' If \code{samp_control = "systematic"} the control locations are randomly generated systematically within the window \code{win} consisting of a grid of equally-spaced points with a random common displacement.
#' 
#' If \code{samp_control = "MVN"} the control locations are randomly generated assuming a multivariate normal distribution centered at coordinates (\code{x_control}, \code{y_control}) with a standard deviation of \code{s_control}.
#' 
#' If \code{samp_control = "CSR"} the control locations are randomly generated assuming complete spatial randomness (homogeneous Poisson process) within the window \code{win} with a \code{lambda = n_control / [resolution x resolution]} By default, the resolution is an integer value of 128 and can be specified using the \code{resolution} argument in the internally called \code{\link[sparr]{risk}} function.
#' 
#' If \code{samp_control = "IPP"} the control locations are randomly generated assuming an inhomogeneous Poisson process within the window \code{win} with a \code{lambda = l_control}, a function.
#' 
#' If \code{samp_control = "clustered"} the control locations are randomly generated with a realisation of the Neyman-Scott process within the window \code{win} with the intensity of the Poisson process cluster centres (\code{kappa = l_control}), the size of the expansion of the simulation window for generative parent points (\code{e_control}), and the radius (or radii) of the disc for each cluster (\code{r_control}).
#' 
#' @return An object of class "list". This is a named list with the following components:
#' 
#' #' \describe{
#' \item{\code{sim}}{An object of class 'rrs' for the first iteration of simulated data.}
#' \item{\code{out}}{An object of class 'rrs' for the observed spatial relative risk function without randomization.}
#' \item{\code{rr_mean}}{Vector of length \code{[resolution x resolution]} of the mean relative risk values at each gridded knot.}
#' \item{\code{pval_mean}}{Vector of length \code{[resolution x resolution]} of the mean asymptotic p-value at each gridded knot.}
#' \item{\code{rr_sd}}{Vector of length \code{[resolution x resolution]} of the standard deviation of relative risk values at each gridded knot.}
#' \item{\code{rr_mean}}{Vector of length \code{[resolution x resolution]} of the proportion of asymptotic p-values that were significant at each gridded knot.}
#' \item{\code{rx}}{Vector of length \code{[resolution x resolution]} of the x-coordinates of each gridded knot.}
#' \item{\code{ry}}{Vector of length \code{[resolution x resolution]} of the y-coordinates of each gridded knot.}
#' \item{\code{rx}}{Vector of length \code{sim_total} of the number of control locations simulated in each iteration.}
#' \item{\code{bandw}}{Vector of length \code{sim_total} of the bandwidth (of numerator) used in each iteration.}
#' \item{\code{bandw}}{Vector of length \code{sim_total} of the global s statistic.}
#' \item{\code{bandw}}{Vector of length \code{sim_total} of the global t statistic.}
#' }
#' 
#' @importFrom stats rnorm sd
#' @importFrom spatstat disc marks ppp rNeymanScott rpoispp rsyst runifdisc runifpoint shift superimpose unit.square
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom foreach %do% %dopar% foreach
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom sparr risk
#' @export
#'
#' @examples
#'  spatial_power(x_case = c(0.25, 0.5, 0.75),
#'                y_case = c(0.75, 0.25, 0.75),
#'                samp_case = "MVN", 
#'                samp_control = "MVN",
#'                x_control = c(0.25, 0.5, 0.75),
#'                y_control = c(0.75, 0.25, 0.75),
#'                n_case = 100,
#'                n_control = c(100,500,300),
#'                s_case = c(0.05,0.01,0.05),
#'                s_control = 0.05,
#'                verbose = FALSE
#'                )
#' 
spatial_power <- function(win = spatstat::unit.square(),
                          sim_total = 2,
                          x_case, y_case,
                          samp_case = c("uniform", "MVN", "CSR", "IPP"),
                          samp_control = c("uniform", "systematic", "MVN",
                                           "CSR", "IPP", "clustered"), 
                          x_control = NULL, y_control = NULL,
                          n_case = NULL, n_control = NULL,
                          npc_control = NULL,
                          r_case = NULL, r_control = NULL,
                          s_case = NULL, s_control = NULL,
                          l_case = NULL, l_control = NULL,
                          e_control = NULL,
                          lower_tail = 0.025,
                          upper_tail = 0.975, 
                          cascon = FALSE,
                          verbose = TRUE,
                          parallel = FALSE,
                          n_core = 2,
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
  
  # Inputs
  if (length(x_case) != length(y_case)) {
    stop("There is at least one missing coordinate")
  }
  
  if (length(n_case) == 1) {
    l <- vector('list', length(x_case))
    for (i in 1:length(x_case)) {
      l[[i]] <- n_case
    }
    n_case <- unlist(l)
  }
  
  if (length(r_case) == 1) {
    l <- vector('list', length(x_case))
    for (i in 1:length(x_case)) {
      l[[i]] <- r_case
    }
    r_case <- unlist(l)
  }
  
  if (length(s_case) == 1) {
    l <- vector('list', length(x_case))
    for (i in 1:length(x_case)) {
      l[[i]] <- s_case
    }
    s_case <- unlist(l)
  }
  
  if (length(l_case) == 1) {
    l <- vector('list', length(x_case))
    for (i in 1:length(x_case)) {
      l[[i]] <- l_case
    }
    l_case <- l
  }
  
  if (samp_control == "MVN" & length(n_control) == 1) {
    l <- vector('list', length(x_control))
    for (i in 1:length(x_control)) {
      l[[i]] <- round(n_control/length(x_control))
    }
    n_control <- unlist(l)
  }
  
  if (length(s_control) == 1) {
    l <- vector('list', length(x_control))
    for (i in 1:length(x_control)) {
      l[[i]] <- s_control
    }
    s_control <- unlist(l)
  }
  
  # marked uniform disc ppp with user-specified radius for cases
  rcluster_case <- function(x0, y0, rad, n, scalar, lamb, wind, types = "case", ...) {
    
    if (samp_case == "uniform"){
      x <- spatstat::runifdisc(n = n, radius = rad, centre = c(x0, y0), win = wind, ...)
    }  
    
    if (samp_case == "MVN"){
      x1 <- rep(x0, n)
      y1 <- rep(y0, n)
      x2 <- x1 + stats::rnorm(n, 0, scalar) 
      y2 <- y1 + stats::rnorm(n, 0, scalar) 
      x <- spatstat::ppp(x2, y2, window = wind)
    }  
    
    if (samp_case == "CSR"){
      win_case <- spatstat::disc(radius = rad, centre = c(0.5, 0.5), ...)
      l <- n/(diff(win_case$xrange)*diff(win_case$yrange))
      x <- spatstat::rpoispp(lambda = l, win = win_case, ...)
      x <- spatstat::shift(x, c(x0 - 0.5, y0 - 0.5))
    }
    
    if (samp_case == "IPP"){
      if (class(lamb) != "function") {
        stop("The argument 'l_case' should be an intensity function")
      }
      win_case <- spatstat::disc(radius = rad, centre = c(0.5, 0.5), ...)
      x <- spatstat::rpoispp(lambda = lamb, win = win_case, ...)
      x <- spatstat::shift(x, c(x0 - 0.5, y0 - 0.5))
    }
    
    spatstat::marks(x) <- types
    return(x)
  }
  
  # marked uniform ppp for controls
  rcluster_control <- function(x0, y0, scalar, n, lamb, ex, nclust, rad, types = "control", wind, ...) {
    if (samp_control == "uniform"){ 
      x <- spatstat::runifpoint(n, win = wind, ...) 
    }
    
    if (samp_control == "systematic") {
      x <- spatstat::rsyst(nx = sqrt(n), win = wind, ...)
    }
    
    if (samp_control == "MVN"){
      x1 <- rep(x0, n)
      y1 <- rep(y0, n)
      x2 <- x1 + stats::rnorm(n, 0, scalar) 
      y2 <- y1 + stats::rnorm(n, 0, scalar) 
      x <- spatstat::ppp(x2, y2, window = wind)
    }  
    
    if (samp_control == "CSR") {
      l <- n/(diff(wind$xrange)*diff(wind$yrange))
      x <- spatstat::rpoispp(lambda = l, win = wind, ...)
    }
    
    if (samp_control == "IPP") {
      if (class(lamb) != "function") {
        stop("The argument 'l_control' should be an intensity function")
      }
      x <- spatstat::rpoispp(lambda = lamb, win = wind, ...)
    }
    
    if (samp_control == "clustered") {
      control_clustering <- function(x0, y0, radius, n) {
        X <- spatstat::runifdisc(n, radius, centre = c(x0, y0))
        return(X)
      }
      x <- spatstat::rNeymanScott(kappa = lamb,
                                  expand = ex,
                                  rcluster = control_clustering, 
                                  n = nclust,
                                  radius = rad,
                                  win = wind,
                                  ...)
    }
    spatstat::marks(x) <- types
    return(x)
  }
  
  # Create empty list
  pppCase <- vector('list', length(x_case))
  
  # Create a consistent random cluster of cases (uniform around user-specified centroid)
  for (i in 1:length(x_case)){
    suppressWarnings(
      x1 <- rcluster_case(x0 = x_case[i], y0 = y_case[i],
                          rad = r_case[i], n = n_case[i],
                          scalar = s_case[i], lamb = l_case[[i]],
                          wind = win, ...)
    )
    pppCase[[i]] <- x1
  }
  class(pppCase) <- c("ppplist", "solist",  "anylist", "listof", "list")
  cas <- spatstat::superimpose(pppCase)
  
  # Progress bar
  if (verbose == TRUE & parallel == FALSE){
    message("Generating Data, Estimating Relative Risk, Calculating Power")
    pb <- txtProgressBar(min = 0, max = sim_total, style = 3)
  }
  
  ## Set function used in foreach
  if (parallel == TRUE){
    loadedPackages <- c("doParallel", "parallel")
    invisible(lapply(loadedPackages, require, character.only = TRUE))
    cl <- parallel::makeCluster(n_core)
    doParallel::registerDoParallel(cl)
    `%fun%` <- foreach::`%dopar%`
  } else {
    `%fun%` <- foreach::`%do%`
  }
  
  # Iteratively calculate the log relative risk and asymptotic p-value surfaces
  out_par <- foreach::foreach(k = 1:sim_total, 
                              .combine = comb, 
                              .multicombine = TRUE, 
                              .packages = c("sparr", "spatstat"),
                              .init = list(list(), list(), list(),
                                           list(), list(), list(),
                                           list(), list(), list(),
                                           list(), list())
  ) %fun% {
    
    # Progress bar
    if (verbose == TRUE & parallel == FALSE){
      setTxtProgressBar(pb, k)
    }
    
    # Create empty list                           
    pppControl <- vector('list', length(x_control))
    
    # Create random cluster of controls
    if(samp_control == "MVN") {
      for (i in 1:length(x_control)){
        suppressWarnings(
          y1 <- rcluster_control(x0 = x_control[i], y0 = y_control[i],
                                 radius = NULL, n = n_control[i],
                                 scalar = s_control[i], lamb =NULL,
                                 wind = win, ...)
        )
        pppControl[[i]] <- y1
      }
      class(pppControl) <- c("ppplist", "solist",  "anylist", "listof", "list")
      con <- spatstat::superimpose(pppControl)
      
    } else { 
      suppressWarnings(
        con <- rcluster_control(x0 = NULL, y0 = NULL,
                                n = n_control, 
                                nclust = npc_control,
                                rad = r_control,
                                ex = e_control,
                                lamb = l_control,
                                scalar = NULL,
                                wind = win,
                                ...)
      )
    }
    
    # Combine random clusters of cases and controls into one marked ppp
    z <- spatstat::superimpose(con, cas)
    spatstat::marks(z) <- as.factor(spatstat::marks(z))
    
    # Calculate observed kernel density ratio
    obs_lrr <- sparr::risk(z, tolerate = TRUE, verbose = FALSE, ...)
    
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
                        "n_cas" = cas$n,
                        "bandw" = obs_lrr$f$h0,
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
                  "n_cas" = unlist(out_par[[8]]),
                  "bandw" = unlist(out_par[[9]]),
                  "s_obs" = unlist(out_par[[10]]),
                  "t_obs" = unlist(out_par[[11]])
  )
}
# -------------------- END OF CODE -------------------- #