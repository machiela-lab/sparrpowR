#' Function to Simulate and Combine User-Specified Case Clusters and Randomized Control Clusters
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
#' @param win 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
spatial_data <- function(x_case, y_case,
                         x_control = NULL, y_control = NULL,
                         n_case = NULL, n_control = NULL, npc_control = NULL,
                         r_case = NULL, r_control = NULL,
                         s_case = NULL, s_control = NULL,
                         l_case = NULL, l_control = NULL, 
                         e_control = NULL,
                         sim_total,
                         samp_case = c("uniform", "MVN", "CSR", "IPP"),
                         samp_control = c("uniform", "systematic","MVN",
                                          "CSR","IPP", "clustered"),
                         win = unit.square(),
                         ...) {
  
  # Package(s)
  require(spatstat)
  
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
  
  # Create empty lists
  pppCase <- vector('list', length(x_case))
  pppControl <- vector('list', length(x_control))
  pppList <- vector('list', length(sim_total))
  
  # Create a consistent random cluster of cases (uniform around user-specified centroid)
  for (i in 1:length(x_case)){
      x1 <- rcluster_case(x0 = x_case[i], y0 = y_case[i],
                          rad = r_case[i], n = n_case[i],
                          scalar = s_case[i], lamb = l_case[[i]],
                          wind = win, ...)
    pppCase[[i]] <- x1
  }
  class(pppCase) <- c("ppplist", "solist",  "anylist", "listof", "list")
  x <- spatstat::superimpose(pppCase)
  
  # Simulate marked ppp for each iteration
  for (j in 1:sim_total) {
    
    # Create random cluster of controls
    if(samp_control == "MVN") {
      for (i in 1:length(x_control)){
        y1 <- rcluster_control(x0 = x_control[i], y0 = y_control[i],
                            radius = NULL, n = n_control[i],
                            scalar = s_control[i], lamb =NULL,
                            wind = win, ...)
        pppControl[[i]] <- y1
      }
      class(pppControl) <- c("ppplist", "solist",  "anylist", "listof", "list")
      y <- spatstat::superimpose(pppControl)
      
    } else { 
      
    y <- rcluster_control(x0 = NULL, y0 = NULL,
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
    z <- spatstat::superimpose(y, x)
    
    # Compile ppp into list
    pppList[[j]] <- z
    }
  class(pppList) <- c("ppplist", "solist",  "anylist", "listof", "list")
  
  return(pppList)
}
# -------------------- END OF CODE -------------------- #