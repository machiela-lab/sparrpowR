# ------------------------------------------ #
# Example input and output for CBIIT Development Tool
#
# Created by: Ian Buller, Ph.D., M.A.
# Created on: April 6, 2020
#
# Recently modified by: 
# Recently modified on: 
#
# Notes:
# A) Requested by Kailing Chen of NIH/NCI/CBIIT
# B) For Brown & Buller DCEG tool challenge proposal 
# ------------------------------------------ #

############
# Packages #
############

library(raster)
library(sp)

###################
# Spatial Objects #
###################

# Create square polygon
x_coord <- c(0,9,9,0)
y_coord <- c(0,0,9,9)
xym <- cbind(x_coord, y_coord)
p <- Polygon(xym)
ps <- Polygons(list(p),1)
sps <- SpatialPolygons(list(ps))

# Create random points
random_cas <- sp::spsample(sps, n = 20, "random") # 20% prevalence
random_con <- sp::spsample(sps, n = 100, "random")

plot(sps)
plot(random_con, col = "blue", pch = 16, cex = 0.5, add = T)
plot(random_cas, col = "red", pch = 16, cex = 0.5, add = T)

# Create randomly clustered points
cluster_cas <- sp::spsample(sps, n = 20, "clustered", nclusters = 1, pretty = T, niter = 20) # 20% prevalence
cluster_con <- sp::spsample(sps, n = 100, "clustered", nclusters = 10, pretty = T, niter = 100)

plot(sps)
plot(cluster_con, col = "blue", pch = 16, cex = 0.5, add = T)
plot(cluster_cas, col = "red", pch = 16, cex = 0.5, add = T)

# Create random raster
x <- raster(ncol=3, nrow=3, xmn=0, xmx=9, ymn=0, ymx=9)
values(x) <- rnorm(9, mean = 0.8, sd = 0.08) 

# Reclassify random raster
## Values > 0.8 = 1 else = 0
x_reclass <- raster::reclassify(x, matrix(c(-Inf, 0.8,0,
                                            0.8, Inf, 1
                                            ),
                                          ncol = 3, byrow = T
                                          )
                                )

######################
# Data Visualization #
######################

load(file = "data/CBIIT_example.Rdata")

# Plot 1: Example Input
grDevices::png(file = paste(getwd(),"/figures/input.png", sep = ""), height = 1000/2, width = 1000/2)
plot(sps, main = "Example Input")
plot(random_con, col = "blue", pch = 16, cex = 1, add = T)
plot(cluster_cas, col = "red", pch = 16, cex = 1, add = T)
legend(xpd = T, x = 3, y = -0.5,
       legend = c("control", "case"),
       col = c("blue", "red"),
       pch = 16,
       bty = "n", 
       ncol = 2
)
dev.off()

# Plot 2: Example Output (continuous)
grDevices::png(file = paste(getwd(),"/figures/output_continuous.png", sep = ""), height = 1000/2, width = 1000/2)
plot(x, axes = F, bty = "n", box = F, main = "Continuous Output")
plot(random_con, col = "blue", pch = 16, cex = 1, add = T)
plot(cluster_cas, col = "red", pch = 16, cex = 1, add = T)
legend(xpd = T, x = 3, y = -0.5,
       legend = c("control", "case"),
       col = c("blue", "red"),
       pch = 16,
       bty = "n", 
       ncol = 2
)
dev.off()

# Plot 3: Example Output (categorical)
grDevices::png(file = paste(getwd(),"/figures/output_categorical.png", sep = ""), height = 1000/2, width = 1000/2)
plot(x_reclass, axes = F, bty = "n", box = F, main = "Categorical Output", legend = F, col = c("grey", "black"))
plot(random_con, col = "blue", pch = 16, cex = 1, add = T)
plot(cluster_cas, col = "red", pch = 16, cex = 1, add = T)
legend(xpd = T, x = 1, y = -0.5,
       legend = c("control", "case", "sufficiently powered", "insufficiently powered"),
       col = c("blue", "red", "black", "grey"),
       pch = c(16, 16, 15, 15),
       bty = "n", 
       ncol = 2
)
dev.off()

####################
# Data Exportation #
####################

save(x, random_con, cluster_cas, sps, file = paste(getwd(), "/data/CBIIT_example.Rdata", sep = ""))

# -------------------- END OF CODE -------------------- #