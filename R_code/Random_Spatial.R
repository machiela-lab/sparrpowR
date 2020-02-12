


# generate autocorrelated data.
nLags <- 75 # number of lags (size of region
)
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
g.dummy <- gstat(formula = z ~ 1 + x + y, 
                 locations = ~ x + y,
                 dummy = T,
                 beta = c(1, 0.01, 0.005), 
                 model = vgm(psill = 0.025,
                             model = "Exp",
                             range = 10
                 ),
                 nmax = 20
)

# make four simulations based on the stat object
yy <- predict(g.dummy, newdata=xy, nsim=1)

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





