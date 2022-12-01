#' Power of SRR function for randomly generated data.
#' 
#' Compute the statistical power of a spatial relative risk function using randomly generated data.
#'
#' @param win Window in which to simulate the random data. An object of class "owin" or something acceptable to \code{\link[spatstat.geom]{as.owin}}.
#' @param sim_total Integer, specifying the number of simulation iterations to perform.
#' @param x_case Numeric value, or numeric vector, of x-coordinate(s) of case cluster(s).
#' @param y_case Numeric value, or numeric vector, of y-coordinate(s) of case cluster(s).
#' @param samp_case Character string specifying whether to randomize the case locations uniformly (\code{samp_control="uniform"}), multivariate normal (\code{samp_control="MVN"}), with complete spatial randomness (\code{samp_control="CSR"}), or using the inhomogeneous Poisson process (\code{samp_control="IPP"}) around each case centroid.
#' @param samp_control Character string specifying whether to randomize the control locations uniformly (\code{samp_control="uniform"}), systematically (\code{samp_control="systematic"}), multivariate normal (\code{samp_control="MVN"}), with complete spatial randomness (\code{samp_control="CSR"}), using the inhomogeneous Poisson process (\code{samp_control="IPP"}), or a realization of the Neyman-Scott cluster process (\code{samp_control="clustered"}).
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
#' @param alpha Optional. Numeric value of the critical p-value (default=0.05).
#' @param p_correct Optional. Character string specifying whether to apply a correction for multiple comparisons including a False Discovery Rate \code{p_correct = "FDR"}, a Sidak correction \code{p_correct = "uncorrelated Sidak"}, and a Bonferroni correction \code{p_correct = "uncorrelated Bonferroni"}. If \code{p_correct = "none"} (the default), then no correction is applied. 
#' @param parallel Logical. If TRUE, will execute the function in parallel. If FALSE (the default), will not execute the function in parallel.
#' @param n_core Optional. Integer specifying the number of CPU cores on current host to use for parallelization (the default is 2 cores).
#' @param verbose Logical. If TRUE (the default), will print function progress during execution. If FALSE, will not print.
#' @param ... Arguments passed to \code{\link[spatstat.random]{runifdisc}}, \code{\link[spatstat.geom]{disc}}, \code{\link[spatstat.random]{rpoispp}}, \code{\link[spatstat.geom]{rsyst}}, or \code{\link[spatstat.random]{rNeymanScott}} depending on \code{samp_control} or \code{samp_control}. Arguments also passed to \code{\link[sparr]{risk}} to select bandwidth, edge correction, and resolution.
#' @param cascon `r lifecycle::badge("deprecated")` \code{cascon} is no longer supported and this function will output power for case-only and case/control clustering. This argument has been moved to \code{spatial_plots} function.
#' @param lower_tail `r lifecycle::badge("deprecated")` \code{lower_tail} is no longer supported; this function uses \code{alpha} to set the critical p-value. 
#' @param upper_tail `r lifecycle::badge("deprecated")` \code{lupper_tail} is no longer supported; this function uses \code{alpha} to set the critical p-value. 
#'
#' @details This function computes the statistical power of the spatial relative risk function (nonparametric estimate of relative risk by kernel smoothing) for randomly generated data using various random point pattern generators from the \code{\link{spatstat.random}} package.
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
#' If \code{samp_control = "clustered"} the control locations are randomly generated with a realization of the Neyman-Scott process within the window \code{win} with the intensity of the Poisson process cluster centres (\code{kappa = l_control}), the size of the expansion of the simulation window for generative parent points (\code{e_control}), and the radius (or radii) of the disc for each cluster (\code{r_control}).
#' 
#' The function computes a one-sided hypothesis test for case clustering (\code{alpha = 0.05} by default). The function also computes a two-sided hypothesis test for case clustering and control clustering (lower tail = 0.025 and upper tail = 0.975).
#' 
#' The function has functionality for a correction for multiple testing. If \code{p_correct = "FDR"}, calculates a False Discovery Rate by Benjamini and Hochberg. If \code{p_correct = "Sidak"}, calculates a Sidak correction. If \code{p_correct = "Bonferroni"}, calculates a Bonferroni correction. If \code{p_correct = "none"} (the default), then the function does not account for multiple testing and uses the uncorrected \code{alpha} level. See the internal \code{pval_correct} function documentation for more details.
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
#' \item{\code{alpha}}{Vector of length \code{sim_total} of the (un)corrected critical p-values.}
#' }
#' 

#' @importFrom doFuture registerDoFuture
#' @importFrom doRNG %dorng%
#' @importFrom foreach %do% %dopar% foreach setDoPar
#' @importFrom future multisession plan
#' @importFrom iterators icount
#' @importFrom lifecycle badge deprecate_warn deprecated is_present
#' @importFrom sparr risk
#' @importFrom spatstat.random rNeymanScott rpoispp runifdisc runifpoint
#' @importFrom spatstat.geom as.solist disc marks ppp rsyst shift superimpose unit.square
#' @importFrom stats rnorm sd
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
#'                verbose = FALSE)
#' 
spatial_power <- function(win = spatstat.geom::unit.square(),
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
                          alpha = 0.05,
                          p_correct = "none",
                          verbose = TRUE,
                          parallel = FALSE,
                          n_core = 2,
                          ...,
                          cascon = lifecycle::deprecated(),
                          lower_tail = lifecycle::deprecated(),
                          upper_tail = lifecycle::deprecated()) {
  
  # Checks
  ## deprecate
  if (lifecycle::is_present(cascon)) {
    lifecycle::deprecate_warn("0.2.0", "sparrpowR::spatial_power(cascon = )")
  }
  if (lifecycle::is_present(lower_tail)) {
    lifecycle::deprecate_warn("0.2.0", "sparrpowR::spatial_power(lower_tail = )")
  }
  if (lifecycle::is_present(upper_tail)) {
    lifecycle::deprecate_warn("0.2.0", "sparrpowR::spatial_power(upper_tail = )")
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
  
  match.arg(p_correct, choices = c("none", "FDR", "Sidak", "Bonferroni"))
  
  # marked uniform disc ppp with user-specified radius for cases
  rcluster_case <- function(x0, y0, rad, n, scalar, lamb, win, types = "case", ...) {
    
    if (samp_case == "uniform") {
      x <- spatstat.random::runifdisc(n = n, radius = rad, centre = c(x0, y0), win = win, ...)
    }  
    
    if (samp_case == "MVN") {
      x1 <- rep(x0, n)
      y1 <- rep(y0, n)
      x2 <- x1 + stats::rnorm(n, 0, scalar) 
      y2 <- y1 + stats::rnorm(n, 0, scalar) 
      x <- spatstat.geom::ppp(x2, y2, window = win)
     }  
    
    if (samp_case == "CSR") {
      win_case <- spatstat.geom::disc(radius = rad, centre = c(0.5, 0.5), ...)
      l <- n/(diff(win_case$xrange)*diff(win_case$yrange))
      x <- spatstat.random::rpoispp(lambda = l, win = win_case, ...)
      x <- spatstat.geom::shift(x, c(x0 - 0.5, y0 - 0.5))
     }
    
    if (samp_case == "IPP") {
      if (!inherits(lamb, "function")) {
        stop("The argument 'l_case' should be an intensity function")
       }
      win_case <- spatstat.geom::disc(radius = rad, centre = c(0.5, 0.5), ...)
      x <- spatstat.random::rpoispp(lambda = lamb, win = win_case, ...)
      x <- spatstat.geom::shift(x, c(x0 - 0.5, y0 - 0.5))
     }
    
    spatstat.geom::marks(x) <- types
    return(x)
   }
  
  # marked uniform ppp for controls
  rcluster_control <- function(x0, y0, scalar, n, lamb, ex, nclust, rad, types = "control", win, ...) {
    if (samp_control == "uniform") { 
      x <- spatstat.random::runifpoint(n, win = win, ...) 
     }
    
    if (samp_control == "systematic") {
      x <- spatstat.geom::rsyst(nx = sqrt(n), win = win, ...)
     }
    
    if (samp_control == "MVN") {
      x1 <- rep(x0, n)
      y1 <- rep(y0, n)
      x2 <- x1 + stats::rnorm(n, 0, scalar) 
      y2 <- y1 + stats::rnorm(n, 0, scalar) 
      x <- spatstat.geom::ppp(x2, y2, window = win)
     }  
    
    if (samp_control == "CSR") {
      l <- n / (diff(win$xrange) * diff(win$yrange))
      x <- spatstat.random::rpoispp(lambda = l, win = win, ...)
     }
    
    if (samp_control == "IPP") {
      if (!inherits(lamb, "function")) {
        stop("The argument 'l_control' should be an intensity function")
      }
      x <- spatstat.random::rpoispp(lambda = lamb, win = win, ...)
    }
    
    if (samp_control == "clustered") {
      control_clustering <- function(x0, y0, radius, n) {
        X <- spatstat.random::runifdisc(n, radius, centre = c(x0, y0))
        return(X)
       }
      x <- spatstat.random::rNeymanScott(kappa = lamb,
                                       expand = ex,
                                       rcluster = control_clustering, 
                                       n = nclust,
                                       radius = rad,
                                       win = win,
                                       ...)
     }
    spatstat.geom::marks(x) <- types
    return(x)
  }
  
  # Create empty list
  pppCase <- vector('list', length(x_case))
  
  # Create a consistent random cluster of cases (uniform around user-specified centroid)
  for (i in 1:length(x_case)) {
    suppressWarnings(
      x1 <- rcluster_case(x0 = x_case[i], y0 = y_case[i],
                          rad = r_case[i], n = n_case[i],
                          scalar = s_case[i], lamb = l_case[[i]],
                          win = win, ...))
    pppCase[[i]] <- x1
  }
  pppCase <- spatstat.geom::as.solist(pppCase)
  cas <- spatstat.geom::superimpose(pppCase)
  
  ## Set function used in foreach
  if (parallel == TRUE) {
    oldplan <- doFuture::registerDoFuture()
    on.exit(with(oldplan, foreach::setDoPar(fun=fun, data=data, info=info)), add = TRUE)
    future::plan(future::multisession, workers = n_core)
    `%fun%` <- doRNG::`%dorng%`
  } else { `%fun%` <- foreach::`%do%` }
  
  # Iteratively calculate the log relative risk and asymptotic p-value surfaces
  out_par <- foreach::foreach(k = 1:sim_total,
                              kk = iterators::icount(),
                              .combine = comb, 
                              .multicombine = TRUE, 
                              .init = list(list(), list(), list(),
                                           list(), list(), list(),
                                           list(), list(), list(),
                                           list(), list(), list(),
                                           list(), list())) %fun% {
    
    # Progress bar
    if (verbose == TRUE) { progBar(kk, sim_total) }
                                             
    # Create empty list                           
    pppControl <- vector('list', length(x_control))
    
    # Create random cluster of controls
    if (samp_control == "MVN") {
      for (i in 1:length(x_control)) {
        suppressWarnings(
          y1 <- rcluster_control(x0 = x_control[i],
                                 y0 = y_control[i],
                                 radius = NULL,
                                 n = n_control[i],
                                 scalar = s_control[i],
                                 lamb =NULL,
                                 win = win, ...))
        pppControl[[i]] <- y1
       }
      pppControl <- spatstat.geom::as.solist(pppControl)
      con <- spatstat.geom::superimpose(pppControl)
    } else { 
      suppressWarnings(
        con <- rcluster_control(x0 = NULL, y0 = NULL,
                                n = n_control, 
                                nclust = npc_control,
                                rad = r_control,
                                ex = e_control,
                                lamb = l_control,
                                scalar = NULL,
                                win = win,
                                ...))
      }
    
    # Combine random clusters of cases and controls into one marked ppp
    z <- spatstat.geom::superimpose(con, cas)
    spatstat.geom::marks(z) <- as.factor(spatstat.geom::marks(z))
    
    # Calculate observed kernel density ratio
    obs_lrr <- sparr::risk(z, tolerate = TRUE, verbose = FALSE, ...)
    
    # Output processing for visualization and summary across iterations
    ## Convert output matrix to two output vectors
    ### Coordinates for each knot
    rx <- rep(obs_lrr$rr$xcol, length(obs_lrr$rr$yrow))
    for(i in 1:length(obs_lrr$rr$yrow)) {
      if (i == 1) { ry <- rep(obs_lrr$rr$yrow[i], length(obs_lrr$rr$xcol)) }
      if (i != 1) { ry <- c(ry, rep(obs_lrr$rr$yrow[i], length(obs_lrr$rr$xcol))) }
    } 
    
    ### Estimated value (log relative risk and p-value) for each knot
    sim_risk <- as.vector(t(obs_lrr$rr$v))
    sim_pval <- as.vector(t(obs_lrr$P$v))
    
    if (p_correct != "none") {
      alpha_correct <- pval_correct(input = obs_lrr, type = p_correct, alpha = alpha)
      
      #### Case and Control (two-tailed test)
      lower_tail <- alpha_correct/2
      upper_tail <- 1 - lower_tail
      pval_sig_cascon <- obs_lrr$P < lower_tail | obs_lrr$P > upper_tail
      pval_sig_cascon <- as.vector(as.numeric(t(pval_sig_cascon$v)))
      #### Case only (one-tailed test, lower tail only)
      pval_sig_cas <- obs_lrr$P < alpha_correct
      pval_sig_cas <- as.vector(as.numeric(t(pval_sig_cas$v)))
    } else {
      alpha_correct <- alpha
      pval_sig_cascon <- "Uncorrected"
      pval_sig_cas <- "Uncorrected"
    }
    
    ### Estimated global test statistics
    #### Global maximum relative risk: H0 = 1
    s_obs <- max(exp(obs_lrr$rr$v[!is.na(obs_lrr$rr$v) & is.finite(obs_lrr$rr$v)]))
    #### Approximation for integral: H0 = 0
    t_obs <- sum((obs_lrr$rr$v[!is.na(obs_lrr$rr$v) & is.finite(obs_lrr$rr$v)] / 
                    (obs_lrr$rr$xstep * obs_lrr$rr$ystep)) ^ 2)
    
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
                        "t_obs" = t_obs,
                        "alpha_correct" = alpha_correct,
                        "pval_sig_cascon" = pval_sig_cascon,
                        "pval_sig_cas" = pval_sig_cas)
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
  ### Correction for multiple testing
  if (p_correct != "none") {
    pval_sig_cascon <- out_par[[13]]
    pval_sig_cas <- out_par[[14]]
  } else {
  ### Uncorrected for multiple testing
  #### Case and Control (lower and upper tail)
  lower_tail <- alpha/2
  upper_tail <- 1 - lower_tail
  pval_sig_cascon <- rapply(sim_pval, function(x) ifelse(x < lower_tail | x > upper_tail,
                                                         TRUE,
                                                         FALSE),
                            how = "replace")
  #### Case only (lower tail only)
  pval_sig_cas <- rapply(sim_pval, function(x) ifelse(x < alpha, TRUE, FALSE),
                         how = "replace")
  }
  
  pval_count_cascon <- rowSums(do.call(cbind, pval_sig_cascon), na.rm = TRUE)
  pval_prop_wNA_cascon <- sapply(pval_count_cascon, FUN = function(x, y = sim_total) (x / y))
  
  pval_count_cas <- rowSums(do.call(cbind, pval_sig_cas), na.rm = TRUE)
  pval_prop_wNA_cas <- sapply(pval_count_cas, FUN = function(x, y = sim_total) (x / y))
  
  ## Force NA values for graphing, match position of NAs of mean p-value
  #### Case and Control (lower and upper tail)
  pval_prop_wNA_cascon <- cbind(pval_mean,pval_prop_wNA_cascon)
  pval_prop_wNA_cascon[ , 2][is.na(pval_prop_wNA_cascon[ , 1])] <- NA
  pval_prop_cascon <- pval_prop_wNA_cascon[ , 2]
  #### Case only (lower tail only)
  pval_prop_wNA_cas <- cbind(pval_mean,pval_prop_wNA_cas)
  pval_prop_wNA_cas[ , 2][is.na(pval_prop_wNA_cas[ , 1])] <- NA
  pval_prop_cas <- pval_prop_wNA_cas[ , 2]
  
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
                  "t_obs" = unlist(out_par[[11]]),
                  "alpha" = unlist(out_par[[12]]))
}
