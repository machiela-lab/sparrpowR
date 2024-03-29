#' Simulate random data for SRR function
#'
#' Generate random two-group data for a spatial relative risk function.
#'
#' @param win Window in which to simulate the random data. An object of class "owin" or something acceptable to \code{\link[spatstat.geom]{as.owin}}.
#' @param sim_total Integer, specifying the number of simulation iterations to perform.
#' @param x_case Numeric value, or numeric vector, of x-coordinate(s) of case cluster(s).
#' @param y_case Numeric value, or numeric vector, of y-coordinate(s) of case cluster(s).
#' @param samp_case Character string specifying whether to randomize the case locations uniformly (\code{samp_control = "uniform"}), multivariate normal (\code{samp_control = "MVN"}), with complete spatial randomness (\code{samp_control = "CSR"}), or using the inhomogeneous Poisson process (\code{samp_control = "IPP"}) around each case centroid.
#' @param samp_control Character string specifying whether to randomize the control locations uniformly (\code{samp_control = "uniform"}), systematically (\code{samp_control = "systematic"}), multivariate normal (\code{samp_control = "MVN"}), with complete spatial randomness (\code{samp_control = "CSR"}), using the inhomogeneous Poisson process (\code{samp_control = "IPP"}), or a realization of the Neyman-Scott cluster process (\code{samp_control = "clustered"}).
#' @param n_case Numeric value, or numeric vector, of the sample size for case locations in each cluster.
#' @param n_control Numeric value, or numeric vector, of the sample size for control locations in each cluster.
#' @param npc_control Optional. Numeric value of the number of clusters of control locations. Ignored if \code{samp_control != "clustered"}.
#' @param x_control Numeric value, or numeric vector, of x-coordinate(s) of case cluster(s). Ignored if \code{samp_control != "MVN"}.
#' @param y_control Numeric value, or numeric vector, of y-coordinate(s) of case cluster(s). Ignored if \code{samp_control != "MVN"}.
#' @param r_case Optional. Numeric value, or numeric vector, of radius (radii) of case cluster(s) in the units of \code{win}. Ignored if \code{samp_case = "MVN"}.
#' @param r_control Optional. Numeric value, or numeric vector, of radius (radii) of control cluster(s) in the units of \code{win}. Ignored if \code{samp_control != "clustered"}.
#' @param s_case Optional. Numeric value, or numeric vector, for the standard deviation(s) of the multivariate normal distribution for case locations in the units of \code{win}. Ignored if \code{samp_control != "MVN"}.
#' @param s_control Optional. Numeric value, or numeric vector, for the standard deviation(s) of the multivariate normal distribution for control locations in the units of \code{win}. Ignored if \code{samp_control != "MVN"}.
#' @param l_case Optional. A single positive number, a vector of positive numbers, a function(x,y, ...), or a pixel image. Intensity of the Poisson process for case clusters. Ignored if \code{samp_control != "IPP"}.
#' @param l_control Optional. A single positive number, a vector of positive numbers, a function(x,y, ...), or a pixel image. Intensity of the Poisson process for control clusters. Ignored if \code{samp_control = "uniform"}, \code{samp_control = "systematic"}, \code{samp_control = "MVN"}, or \code{samp_control = "CSR"}.
#' @param e_control Optional. A single non-negative number for the size of the expansion of the simulation window for generating parent points. Ignored if \code{samp_control != "clustered"}.
#' @param ... Arguments passed to \code{\link[spatstat.random]{runifdisc}}, \code{\link[spatstat.geom]{disc}}, \code{\link[spatstat.random]{rpoispp}}, \code{\link[spatstat.geom]{rsyst}}, or \code{\link[spatstat.random]{rNeymanScott}} depending on \code{samp_control} or \code{samp_control}.
#' 
#' @details This function generates random data for a spatial relative risk function (nonparametric estimate of relative risk by kernel smoothing) using various random point pattern generators from the \code{\link{spatstat.random}} package to generate data.
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
#' If \code{samp_control = "CSR"} the control locations are randomly generated assuming complete spatial randomness (homogeneous Poisson process) within the window \code{win} with a \code{lambda = n_control / [resolution x resolution]}. By default, the resolution is an integer value of 128 and can be specified using the \code{resolution} argument in the internally called \code{\link[sparr]{risk}} function.
#' 
#' If \code{samp_control = "IPP"} the control locations are randomly generated assuming an inhomogeneous Poisson process within the window \code{win} with a \code{lambda = l_control}, a function.
#' 
#' If \code{samp_control = "clustered"} the control locations are randomly generated with a realization of the Neyman-Scott process within the window \code{win} with the intensity of the Poisson process cluster centres (\code{kappa = l_control}), the size of the expansion of the simulation window for generative parent points (\code{e_control}), and the radius (or radii) of the disc for each cluster (\code{r_control}).
#' 
#' @return An object of class "ppplist". This is a list of marked point patterns that have a single mark with two levels: case and control.
#' 
#' @importFrom stats rnorm
#' @importFrom spatstat.random rNeymanScott rpoispp runifdisc runifpoint
#' @importFrom spatstat.geom as.solist disc marks ppp rsyst shift superimpose unit.square
#' @export
#'
#' @seealso \code{\link[spatstat.random]{runifdisc}}, \code{\link[spatstat.geom]{disc}}, \code{\link[spatstat.random]{rpoispp}}, \code{\link[spatstat.geom]{rsyst}}, or \code{\link[spatstat.random]{rNeymanScott}} for additional arguments for random point pattern generation.
#'
#' @examples
#'  spatial_data(x_case = c(0.25, 0.5, 0.75),
#'               y_case = c(0.75, 0.25, 0.75),
#'               samp_case = "MVN", 
#'               samp_control = "MVN",
#'               x_control = c(0.25, 0.5, 0.75),
#'               y_control = c(0.75, 0.25, 0.75),
#'               n_case = 100,
#'               n_control = c(100,500,300),
#'               s_case = c(0.05,0.01,0.05),
#'               s_control = 0.05,
#'               verbose = FALSE)
#'  
#' 
spatial_data <- function(win = spatstat.geom::unit.square(),
                         sim_total = 2,
                         x_case, y_case,
                         samp_case = c("uniform", "MVN", "CSR", "IPP"),
                         samp_control = c("uniform", "systematic","MVN",
                                          "CSR","IPP", "clustered"),
                         x_control = NULL, y_control = NULL,
                         n_case = NULL, n_control = NULL, 
                         npc_control = NULL,
                         r_case = NULL, r_control = NULL,
                         s_case = NULL, s_control = NULL,
                         l_case = NULL, l_control = NULL, 
                         e_control = NULL,
                         ...) {
  
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
      x <- spatstat.random::runifdisc(n = n, radius = rad, centre = c(x0, y0), win = wind, ...)
    }  
    
    if (samp_case == "MVN"){
      x1 <- rep(x0, n)
      y1 <- rep(y0, n)
      x2 <- x1 + stats::rnorm(n, 0, scalar) 
      y2 <- y1 + stats::rnorm(n, 0, scalar) 
      x <- spatstat.geom::ppp(x2, y2, window = wind)
    }  
    
    if (samp_case == "CSR"){
      win_case <- spatstat.geom::disc(radius = rad, centre = c(0.5, 0.5), ...)
      l <- n / (diff(win_case$xrange)*diff(win_case$yrange))
      x <- spatstat.random::rpoispp(lambda = l, win = win_case, ...)
      x <- spatstat.geom::shift(x, c(x0 - 0.5, y0 - 0.5))
    }
    
    if (samp_case == "IPP"){
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
  rcluster_control <- function(x0, y0, scalar, n, lamb, ex, nclust, rad, types = "control", wind, ...) {
    if (samp_control == "uniform"){ 
      x <- spatstat.random::runifpoint(n, win = wind, ...) 
    }
    
    if (samp_control == "systematic") {
      x <- spatstat.geom::rsyst(nx = sqrt(n), win = wind, ...)
    }
    
    if (samp_control == "MVN"){
      x1 <- rep(x0, n)
      y1 <- rep(y0, n)
      x2 <- x1 + stats::rnorm(n, 0, scalar) 
      y2 <- y1 + stats::rnorm(n, 0, scalar) 
      x <- spatstat.geom::ppp(x2, y2, window = wind)
    }  
    
    if (samp_control == "CSR") {
      l <- n / (diff(wind$xrange)*diff(wind$yrange))
      x <- spatstat.random::rpoispp(lambda = l, win = wind, ...)
    }
    
    if (samp_control == "IPP") {
      if (!inherits(lamb, "function")) {
        stop("The argument 'l_control' should be an intensity function")
      }
      x <- spatstat.random::rpoispp(lambda = lamb, win = wind, ...)
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
                                  win = wind,
                                  ...)
    }
    spatstat.geom::marks(x) <- types
    return(x)
  }
  
  # Create empty lists
  pppCase <- vector('list', length(x_case))
  pppControl <- vector('list', length(x_control))
  pppList <- vector('list', length(sim_total))
  
  # Create a consistent random cluster of cases (uniform around user-specified centroid)
  for (i in 1:length(x_case)){
    x1 <- rcluster_case(x0 = x_case[i],
                        y0 = y_case[i],
                        rad = r_case[i],
                        n = n_case[i],
                        scalar = s_case[i],
                        lamb = l_case[[i]],
                        wind = win, ...)
    pppCase[[i]] <- x1
  }
  pppCase <- spatstat.geom::as.solist(pppCase)
  x <- spatstat.geom::superimpose(pppCase)
  
  # Simulate marked ppp for each iteration
  for (j in 1:sim_total) {
    # Create random cluster of controls
    if(samp_control == "MVN") {
      for (i in 1:length(x_control)) {
        y1 <- rcluster_control(x0 = x_control[i],
                               y0 = y_control[i],
                               radius = NULL,
                               n = n_control[i],
                               scalar = s_control[i],
                               lamb =NULL,
                               wind = win, ...)
        pppControl[[i]] <- y1
      }
      pppControl <- spatstat.geom::as.solist(pppControl)
      y <- spatstat.geom::superimpose(pppControl)
    } else { 
      y <- rcluster_control(x0 = NULL,
                            y0 = NULL,
                            n = n_control, 
                            nclust = npc_control,
                            rad = r_control,
                            ex = e_control,
                            lamb = l_control,
                            scalar = NULL,
                            wind = win,
                            ...)
    }
    
    # Combine random clusters of cases and controls into one marked ppp
    z <- spatstat.geom::superimpose(y, x)
    spatstat.geom::marks(z) <- as.factor(spatstat.geom::marks(z))
    
    # Compile ppp into list
    pppList[[j]] <- z
  }
  
  pppList <- spatstat.geom::as.solist(pppList)
  
  return(pppList)
}
