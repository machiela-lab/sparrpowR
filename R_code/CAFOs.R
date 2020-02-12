# ------------------------------------------ #
# Concentrated Agricultural Farm Operations
#
# Created by: Ian Buller, Ph.D., M.A.
# Created on: November 8, 2019
#
# Recently modified by: 
# Recently modified on: 
#
# Notes:
# A) 
# ------------------------------------------ #

############
# SETTINGS #
############

## Path for working directory within L://
ian_path <- "/Volumes/Group02/DCEG_ALL/Jones R/Ian/TSRACT" # at NCI 
ian_path <- "/Volumes/Ian/TSRACT" # on VPN

## Set path
setwd(ian_path)
getwd() # check path set correctly 

## Path for IWHS 2013 Data
cafos_path <- paste(ian_path, "/data/AFOs_CMAQ_022018_v2.csv", sep="")

############
# PACKAGES #
############

# install.packages("dplyr")
# install.packages("maps")
# install.packages("maptools")
# install.packages("raster")
# install.packages("rgeos")
# install.packages("sp")
# install.packages("sparr")
# install.packages("spatstat")
library(dplyr) # for data wrangling
library(maps) # for administrative district polygons
library(maptools) # for as.owin
library(raster) # for finer administrative distric polygons
library(rgeos) # for detecting point outside of boundary polygon
library(sp) # for creating spatial points objects
library(sparr) # for spatial relative risk estimation
library(spatstat) # for ppp objects

####################
# DATA IMPORTATION #
####################

cafos <- read.csv(cafos_path)

## Summary
# n = 11,363 sites
# 64 variables

names(cafos) # names of the variables
str(cafos) # structure of data; NOTE: Data formats were NOT preserved, must clean in later step

summary(cafos$LEGENDTYPE)

plot(x = cafos$Longitude, y = cafos$Latitude,
     ylim = c(40.5,43.5),
     xlab = "Longitude (degrees)",
     ylab = "Latitude (degrees)"
     )
maps::map(database = "state", region = "Iowa", add = T)
legend(xpd = T,
       x = -89.75, y = 41,
       legend = c("AFO", "State Boundary"),
       pch = c(1,NA),
       lty = c(NA,1),
       col = c("black", "black")
)


plot(x = cafos$Longitude[cafos$LEGENDTYPE == 7], y = cafos$Latitude[cafos$LEGENDTYPE == 7],
     ylim = c(40.5,43.5),
     xlab = "Longitude (degrees)",
     ylab = "Latitude (degrees)",
     col = "red"
)
points(x = cafos$Longitude[cafos$LEGENDTYPE == 26], y = cafos$Latitude[cafos$LEGENDTYPE == 26],
       col = "black"
       )
maps::map(database = "state", region = "Iowa", add = T)
legend(xpd = T,
       x = -89.75, y = 41,
       legend = c("Multiple", "AFO"),
       title = "Facility",
       pch = c(1,1),
       col = c("red", "black")
       )

par(pty = "s")
plot(x = cafos$Longitude[cafos$Swine > 0 & cafos$CattleBeef == 0], y = cafos$Latitude[cafos$Swine > 0 & cafos$CattleBeef == 0],
     ylim = c(40.5,43.5),
     xlab = "Longitude (degrees)",
     ylab = "Latitude (degrees)",
     col = "red"
)
points(x = cafos$Longitude[cafos$Swine == 0 & cafos$CattleBeef > 0], y = cafos$Latitude[cafos$Swine == 0 & cafos$CattleBeef > 0],
       col = "black"
)
maps::map(database = "state", region = "Iowa", add = T)
legend(xpd = T,
       x = -89.75, y = 41,
       legend = c("Swine Only", "Beef Cattle Only"),
       title = "Facility",
       pch = c(1,1),
       col = c("red", "black")
)

####################################
# SPATIAL RELATIVE RISK ESTIMATION #
####################################
states <- raster::getData("GADM", country = "USA", level = 1)
states$OBJECTID <- row.names(states)
iowa <- states[states$NAME_1 %in% "Iowa",] 
ia <- spatstat::as.owin(iowa)

ppp_swine <- spatstat::ppp(x = cafos$Longitude[cafos$Swine > 0 & cafos$CattleBeef == 0],
                           y = cafos$Latitude[cafos$Swine > 0 & cafos$CattleBeef == 0],
                           window = ia
                           )

ppp_bcttl <- spatstat::ppp(x = cafos$Longitude[cafos$Swine == 0 & cafos$CattleBeef > 0],
                           y = cafos$Latitude[cafos$Swine == 0 & cafos$CattleBeef > 0],
                           window = ia
                           )

cafos$SorB <- ifelse(cafos$Swine > 0 & cafos$CattleBeef == 0, 0,
                     ifelse(cafos$Swine == 0 & cafos$CattleBeef > 0 , 1, NA))

ppp_all <- spatstat::ppp(x = cafos$Longitude[(cafos$Swine > 0 & cafos$CattleBeef == 0) | (cafos$Swine == 0 & cafos$CattleBeef > 0)],
                         y = cafos$Latitude[(cafos$Swine > 0 & cafos$CattleBeef == 0) | (cafos$Swine == 0 & cafos$CattleBeef > 0)],
                         window = ia,
                         marks = as.factor(cafos$SorB[(cafos$Swine > 0 & cafos$CattleBeef == 0) | (cafos$Swine == 0 & cafos$CattleBeef > 0)])
                         )

plot(ppp_all)

# Detecting AFO outside of Iowa
p_all <- cbind(cafos$Longitude[(cafos$Swine > 0 & cafos$CattleBeef == 0) | (cafos$Swine == 0 & cafos$CattleBeef > 0)],
               cafos$Latitude[(cafos$Swine > 0 & cafos$CattleBeef == 0) | (cafos$Swine == 0 & cafos$CattleBeef > 0)])
p_sp <- sp::SpatialPoints(p_all, proj4string = CRS(proj4string(iowa)))
p_out <- rgeos::gIntersects(p_sp, iowa, byid = T)

plot(p_sp)
points(p_sp[!p_out,], col='red', cex=5, pch=20)


# Relative Risk
obs_lrr <- sparr::risk(ppp_all, adapt = T, tolerate = T)
plot(obs_lrr)
plot(obs_lrr$P)

rx <- rep(obs_lrr$rr$xcol, length(obs_lrr$rr$yrow))
for(i in 1:length(obs_lrr$rr$yrow)){
        if (i == 1){ry <- rep(obs_lrr$rr$yrow[i], length(obs_lrr$rr$xcol))}
        if (i != 1){ry <- c(ry,rep(obs_lrr$rr$yrow[i], length(obs_lrr$rr$xcol)))}
}

out_lrr <- as.data.frame(dplyr::tibble(x = rx,
                                       y = ry,
                                       lrr = as.vector(t(obs_lrr$rr$v))))
sp::coordinates(out_lrr) <- ~ x + y
sp::gridded(out_lrr) <- TRUE
lrr_raster <- raster::raster(out_lrr)
plot(lrr_raster)

out_pvl <- as.data.frame(dplyr::tibble(x = rx,
                                       y = ry,
                                       lrr = as.vector(t(obs_lrr$P$v))))
sp::coordinates(out_pvl) <- ~ x + y
sp::gridded(out_pvl) <- TRUE
pvl_raster <- raster::raster(out_pvl)
pvl_raster_reclass <- raster::reclassify(pvl_raster, c(-Inf, 0.005, 5,
                                                       0.005, 0.025, 4,
                                                       0.025, 0.975, 3,
                                                       0.975, 0.995, 2,
                                                       0.995, Inf, 1
                                                       )
                                         )
plot(pvl_raster)
plot(pvl_raster_reclass,
     col = c("blue3", "cornflowerblue", "grey80", "indianred1", "indianred4"),
     )
# blue = more likely cattle
# red = more likely swine


