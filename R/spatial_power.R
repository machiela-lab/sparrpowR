#' Function to Estimate the Power of a Spatial Relative Risk using Simulated Data
#'
#' @param x_case 
#' @param y_case 
#' @param x_control 
#' @param y_control 
#' @param n_case 
#' @param n_control 
#' @param npc_control 
#' @param r_case 
#' @param r_control 
#' @param s_case 
#' @param s_control 
#' @param l_case 
#' @param l_control 
#' @param e_control 
#' @param sim_total 
#' @param samp_case 
#' @param samp_control 
#' @param upper_tail 
#' @param lower_tail 
#' @param win 
#' @param cascon 
#' @param resolution 
#' @param edge 
#' @param adapt 
#' @param h0 
#' @param verbose 
#' @param parallel 
#' @param n_core 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
spatial_power <- function(x_case, y_case,
                          x_control = NULL, y_control = NULL,
                          n_case = NULL, n_control = NULL, npc_control = NULL,
                          r_case = NULL, r_control = NULL,
                          s_case = NULL, s_control = NULL,
                          l_case = NULL, l_control = NULL,
                          e_control = NULL,
                          sim_total = 1,
                          samp_case = c("uniform", "MVN", "CSR", "IPP"),
                          samp_control = c("uniform", "systematic", "MVN",
                                           "CSR", "IPP", "clustered"), 
                          upper_tail = 0.975,
                          lower_tail = 0.025,
                          win = unit.square(),
                          cascon = FALSE,
                          resolution = 128,
                          edge = "uniform",
                          adapt = FALSE,
                          h0 = NULL,
                          verbose = TRUE,
                          parallel = FALSE,
                          n_core = NULL,
                          ...) {
  
  # Packages
  loadedPackages <- c("foreach", "sparr", "spatstat")
  invisible(lapply(loadedPackages, require, character.only = T))
  
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
      x2 <- x1 + rnorm(n, 0, scalar) 
      y2 <- y1 + rnorm(n, 0, scalar) 
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
      x2 <- x1 + rnorm(n, 0, scalar) 
      y2 <- y1 + rnorm(n, 0, scalar) 
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
    
    # Bandwidth selection
    if(is.null(h0)){
      h0 <- sparr::OS(z, nstar = "geometric")
    }
    
    # Calculate observed kernel density ratio
    obs_lrr <- sparr::risk(z, tolerate = T, verbose = F,
                           resolution = resolution, ## SEE NOTE (F)
                           edge = edge,
                           adapt = adapt,
                           h0 = h0,
                           ...)
    
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
                  "n_cas" = unlist(out_par[[8]]),
                  "bandw" = unlist(out_par[[9]]),
                  "s_obs" = unlist(out_par[[10]]),
                  "t_obs" = unlist(out_par[[11]])
                  )
  }
# -------------------- END OF CODE -------------------- #