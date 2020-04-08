# ------------------------------------------ #
# Resources to randomly simulate relative spatial clusters
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: January 31, 2020
#
# Recently modified by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Recently modified on: 04/06/2020
#
# Notes:
# A) 4/6/20: Cleaned up for GitHub
# ------------------------------------------ #


##################
# Using SPATSTAT #
##################

# Simulate Stratified Random Point Pattern
test <- spatstat::rstrat(nx=10)
plot(test)

# Simulate systematic random point pattern 
test <- spatstat::rsyst(nx=10)
plot(test)

# 100 uniform random points in the unit square
test <- spatstat::rpoint(100)
plot(test)

# 100 random points with probability density proportional to x^2 + y^2
test <- spatstat::rpoint(100, function(x,y) { x^2 + y^2}, fmax = 1)
plot(test)

# `fmax' may be omitted
test <- spatstat::rpoint(100, function(x,y) { x^2 + y^2})
plot(test)

test_con <- spatstat::rpoint(1000, f = function(x,y) { x^2 + y^2 })
test_cas <- spatstat::rpoint(200, f = function(x,y) { x + y })

plot(spatstat::unit.square(), main = "Example Input")
plot(test_con, pch = 16, cex = 0.5, cols = "blue", add = T)
plot(test_cas, pch = 16, cex = 0.5, cols = "red", add = T)
legend(xpd = T, 
       x = "bottom",
       yjust = -1,
       legend = c("control", "case"),
       col = c("blue", "red"),
       pch = 16,
       bty = "n", 
       ncol = 2
)

# each cluster consist of 10 points in a disc of radius 0.2
nclust <- function(x0, y0, radius, n) {
  return(spatstat::runifdisc(n, radius, centre = c(x0, y0)))
}

nclust2 <- function(x0, y0, radius, n, types=c("case", "control")) {
  X <- spatstat::runifdisc(n, radius, centre=c(x0, y0))
  M <- sample(types, n, replace=TRUE)
  spatstat::marks(X) <- M
  return(X)
}

nclust_cas <- function(x0, y0, radius, n, types = "case") {
  X <- spatstat::runifdisc(n, radius, centre=c(x0, y0))
  spatstat::marks(X) <- types
  return(X)
}

nclust_con <- function(x0, y0, radius, n, types = "control") {
  X <- spatstat::runifdisc(n, radius, centre=c(x0, y0))
  spatstat::marks(X) <- types
  return(X)
}

test <- spatstat::rNeymanScott(kappa = 5, 
                               expand = 0, 
                               win = spatstat::unit.square(),
                               rcluster = nclust, 
                               n = 200,
                               radius = 0.5
                               ); test ; plot(test)

test_y <- spatstat::rlabel(test, labels=factor(c("case", "control")), permute=FALSE)
table(test_y$marks)
plot(test_y, pch = 16, cols = c("red", "blue"))





random.cluster <- function(n_cluster, ...) {
  repeat {
    # do something
    x <- spatstat::rNeymanScott(...)
    # exit if the condition is met
    if (x$n == n_cluster) break
  }
  return(x)
}






rand_case <- random.cluster(kappa = 5, 
                         expand = 0, 
                         win = spatstat::unit.square(),
                         rcluster = nclust_cas, 
                         n = 50,
                         radius = 0.1,
                         n_cluster = 200
                         ); rand_case$n; table(rand_case$marks); plot(rand_case)


rand_control <- random.cluster(kappa = 5, 
                         expand = 0, 
                         win = spatstat::unit.square(),
                         rcluster = nclust_con, 
                         n = 200,
                         radius = 0.5,
                         n_cluster = 1000
                         ); rand_control$n; table(rand_control$marks); plot(rand_control)

random.casecon <- function(prevalence, n_total, n_case, n_control, k_case, k_control, e_case, e_control, r_case, r_control,...) {
  
  ncluster_case <- function(x0, y0, radius, n, types = "case") {
    X <- spatstat::runifdisc(n, radius, centre=c(x0, y0))
    spatstat::marks(X) <- types
    return(X)
  }
  
  ncluster_control <- function(x0, y0, radius, n, types = "control") {
    X <- spatstat::runifdisc(n, radius, centre=c(x0, y0))
    spatstat::marks(X) <- types
    return(X)
  }
  
  repeat {
x <- random.cluster(kappa = k_case,
                            expand = e_case,
                            rcluster = rcluster_case, 
                            n = n_case,
                            radius = r_case,
                            n_cluster = prevalence*n_total
)

y <- random.cluster(kappa = k_control,
                            expand = e_control,
                            rcluster = rcluster_control, 
                            n = n_control,
                            radius = r_control,
                            n_cluster = (1-prevalence)*n_total
                            )

z <- spatstat::superimpose(x, y)

return(z)
}
}

rand_pts <- random.casecon(prevalence = 0.2, 
                           n_total = 1000,
                           n_case = 50,
                           n_control = 200,
                           k_case = 5, 
                           k_control = 5,
                           e_case = 0, 
                           e_control = 0, 
                           r_case = 0.1, 
                           r_control = 0.5
                           ); table(rand_pts$marks); plot(rand_pts, pch = 1, cols = c("red", "blue"))

# Random Point Location
# Random Marks
# 20% prevalence
random.prevalence <- function(n, prev) {
  repeat {
    # do something
    x <- sample(0:1, n, replace = TRUE, prob = c(1-prev/100, prev/100))
    # exit if the condition is met
    if (sum(x) == prev) break
  }
  return(x)
}

prevalence <- 20
n_sample <- 100
x <- runif(n_sample)
y <- runif(n_sample)

mar <- random.prevalence(n = n_sample, prev = prevalence)
mar <- ifelse(mar == 0, "control", "case")
mm <- spatstat::ppp(x, y, c(0,1), c(0,1), marks = as.factor(mar))
plot(mm, pch = 16, cols = c("red", "blue"))

# Poisson Point Location
# Random Marks
# 20% prevalence

# Homogenous Poisson process with intensity 100 in the unit square
random.pp <- function(lambda,...) {
  repeat {
    # do something
    x <- spatstat::rpoispp(lambda)
    # exit if the condition is met
    if (x$n == lambda) break
  }
  return(x)
}

lambda <- 100

pp <- random.pp(lambda, win = spatstat::unit.square())
plot(pp)



# Homogenous Poisson process with intensity 100 in the unit square
random.ipp <- function(lambda, n_set, ...) {
  repeat {
    # do something
    x <- spatstat::rpoispp(lambda, ...)
    # exit if the condition is met
    if (x$n == n_set) break
  }
  return(x)
}

lamb <- function(x,y,a,l0) { l0 * exp( - a * x)}
l0 <- 100

ipp <- spatstat::rpoispp(lambda = lamb, l0 = 100, lmax = 100, a = 3,  win = spatstat::unit.square()); ipp$n
plot(ipp)

ipp <- random.ipp(lambda = lamb, n_set = 50, l0 = 100, lmax = 100, a = 4, win = spatstat::unit.square())
plot(ipp)



# Marked Poisson Point Processe

# uniform bivariate Poisson process with total intensity 100 in unit square
pp <- spatstat::rmpoispp(50, types=c("a","b"))
plot(pp, pch = 16, cols = c("red", "blue"))

# stationary bivariate Poisson process with intensity A = 30, B = 70
pp <- spatstat::rmpoispp(c(30,70), types=c("A","B"))
plot(pp, pch = 16, cols = c("red", "blue"))
pp <- spatstat::rmpoispp(c(30,70))
plot(pp, pch = 16, cols = c("red", "blue"))


# inhomogeneous lambda(x,y,m)
# note argument 'm' is a factor 
lam <- function(x,y,m) { 50 * (x^2 + y^3) * ifelse(m=="A", 2, 1)}
pp <- spatstat::rmpoispp(lam, win=letterR, types=c("A","B"))
plot(pp, pch = 16, cols = c("red", "blue"))

# extra arguments
lam <- function(x,y,m,scal) { scal * (x^2 + y^2) * ifelse(m == "A", 1, 5)}
pp <- spatstat::rmpoispp(lam, win = spatstat::unit.square(), types = c("A","B"), scal = 20)
plot(pp, pch = 16, cols = c("red", "blue"))
table(pp$marks)

set.seed(42)

devtools::install_github("r-spatialecology/onpoint")

input_pattern <- spatstat::rpoispp(lambda = function(x , y) {100 * exp(-3 * x)}, nsim = 1)
null_model <- onpoint::simulate_heterogenous_pattern(input_pattern, nsim = 19)
spatstat::envelope(Y = input_pattern, fun = spatstat::pcf, nsim = 19, simulate = null_model)






str(test)

kappa()


# generate autocorrelated data.
nLags <- 75 # number of lags (size of region)
# fake, uncorrelated observations
X <- rnorm(nLags)

###############################################
# fake sigma... correlated decreases distance.
sigma <- diag(nLags)
corr <- 0.1
sigma <- corr ^ abs(row(sigma)-col(sigma))

###############################################

# Y is autocorrelated...
Y <- t(X %*% chol(sigma))

plot(X,Y)

randf <- function(x,y,...){}
spatstat::rpoint(100, )


# http://santiago.begueria.es/2010/10/generating-spatially-correlated-random-fields-with-r/
install.packages("gstat")
# unconditional simulations on a 100 x 100 grid using gstat
library(gstat)

# create structure
xy <- expand.grid(1:100, 1:100)
names(xy) <- c("x","y")

# define the gstat object (spatial model)
g.dummy <- gstat::gstat(formula=z~1+x+y, 
                 locations=~x+y,
                 dummy=T,
                 beta=c(1,0.01,0.005), 
                 model=vgm(psill=0.025,
                           model="Exp",
                           range=15
                           ),
                 nmax=20
                 )

# make four simulations based on the stat object
yy <- predict(g.dummy, newdata=xy, nsim=1)

# show one realization
sp::gridded(yy) <- ~x+y
sp::spplot(yy[1])

# show all four simulations:
sp::spplot(yy)


# 10 by 10 grid
# create structure
xy <- expand.grid(1:10, 1:10)
names(xy) <- c("x","y")

# define the gstat object (spatial model)
g.dummy <- gstat::gstat(formula = z ~ 1 + x + y, 
                 locations = ~ x + y,
                 dummy = T,
                 beta = c(1, 0.01, 0.005), 
                 model = gstat::vgm(psill = 0.025,
                             model = "Exp",
                             range = 10
                 ),
                 nmax = 20
)

# make four simulations based on the stat object
yy <- predict(g.dummy, newdata=xy, nsim=1)

test <- sp::SpatialPointsDataFrame(yy[1], data = data.frame(yy[1]$sim1))



hist(yy$sim1)

# show one realization
sp::gridded(yy) <- ~x+y
sp::spplot(yy[1])

# show all four simulations:
sp::spplot(yy)

lamb <- function(x, y, a) {100 * exp(- a * x)}
pp <- spatstat::rpoispp(lambda = lamb, lmax = 100, a = 8); pp
plot(pp)

pp <- spatstat::rpoispp(100); pp
plot(pp)


simulate_heterogenous_pattern(pp, nsim = 1)
runifpoispp()

# -------------------- END OF CODE -------------------- #