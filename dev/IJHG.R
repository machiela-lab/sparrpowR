# ----------------------------------------------------------- #
# Code companion for the sparrpowR manuscript in the
# International Journal of Health Geographics
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: January 09, 2021
#
# Co-author: Derek Brown, Ph.D., M.S. (GitHub: @derekbrown12)
#
# Notes:
# A) Includes code for all scenarios and figures in manuscript
# B) Must use your own census key to generate Figure 1A
# ----------------------------------------------------------- #

# Packages
loadedPackages <- c("broom", "cowplot", "ggmap", "ggplot2", "maptools", "parallel", "raster", "rgeos", "sf", "sp", "sparrpowR", "tibble", "tidycensus")
invisible(lapply(loadedPackages, require, character.only = TRUE))

# Setting
## Access Key for census data download
### Obtain one at http://api.census.gov/data/key_signup.html
census_key <- "XXXXXX" # INSERT KEY HERE

## Color for plots
cols <- c("cornflowerblue", "green", "red", "lightgreen", "blue") 
#cols <- c("#6495ED", "#00FF00", "#FF0000", "#90EE90", "#0000FF") # uncomment for hexcodes, if desirable

## Power threshold
p_thresh <- 0.8

# Data Preparation
## CAFO location
CAFO <- data.frame("lon" = -94.20764, "lat" = 42.52006) # Coordinates of CAFO
spdf <- sp::SpatialPointsDataFrame(coords = CAFO[ , 1:2],
                                   data = CAFO, # create SpatialPointsDataFrame
                                   proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
utm15 <- sf::st_transform(sf::st_as_sf(spdf), crs = 32615) # UTM 15 projection of CAFO
## Spatial buffers around CAFO
km5 <- sf::st_buffer(utm15, dist = 5000) # 5 km polygon around CAFO
km10 <- sf::st_buffer(utm15, dist = 10000) # 10 km polygon around CAFO
km5_wgs84 <- sf::st_transform(km5, crs = 4326) # 5 km polygon in WGS84
km10_wgs84 <- sf::st_transform(km10, crs = 4326) # 10 km polygon in WGS84
## Spatial window of analysis
ow <- maptools::as.owin.SpatialPolygons(as(km10, "Spatial")) # window object for spatial_power
## Fort Dodge, Iowa location
FD <- data.frame("lon" = -94.1680, "lat" = 42.4975) # Coordinates of Fort Dodge
spdf_FD <- sp::SpatialPointsDataFrame(coords = FD[ , 1:2],
                                      data = FD, # create SpatialPointsDataFrame
                                      proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
utm15_FD <- sf::st_transform(sf::st_as_sf(spdf_FD), crs = 32615) # UTM 15 projection of Fort Dodge
## Fort Dodge, Iowa population
### 2010 Dicennial U.S. Census of Iowa by tract
ia_tract <- tidycensus::get_decennial(geography = "tract",
                                      variables = c(population = "P001001"),
                                      year = 2010,
                                      state = "Iowa",
                                      geometry = TRUE,
                                      key = census_key)
tract_proj <- sf::st_transform(ia_tract, crs = 32615) # UTM 15 projection of Iowa Census Tracts 
tract_proj$area <- as.numeric(sf::st_area(tract_proj)) # surface area of each tract (sq m)
tract_proj$dens <- tract_proj$value/(tract_proj$area/1000^2) # population density of each tract (sq km)
fd_proj <- tract_proj[tract_proj$GEOID == "19187000400", ] # tract at center of Fort Dodge, Iowa
fd_bb <- sf::st_as_sfc(sf::st_bbox(km10)) # spatial bounding box 
fd_bbb_wgs84 <- sf::st_transform(fd_bb, crs = 4326) # 10 km polygon in WGS84
fd_bb_l <- sf::st_bbox(fd_bbb_wgs84)
names(fd_bb_l) <- c("left", "bottom", "right", "top") # rename for get_map() function
fd_wgs84 <- sf::st_transform(tract_proj, crs = 4326) # tracts (with new features) in WGS84
## Basemap of Fort Dodge, Iowa
fd_base <- ggmap::get_map(location = fd_bb_l, maptype = "toner", source = "stamen") # download stamen base map for the bounding box

## Basemap of Fort Dodge, Iowa
gg1a <- ggmap::ggmap(fd_base) + # basemap
        ggplot2::geom_point(data = CAFO, # CAFO location
                            ggplot2::aes(x = lon, y = lat),
                            shape = 13,
                            size = 5,
                            stroke = 1) +
        ggplot2::geom_sf(data = km5_wgs84, # original boundary
                         fill = "transparent",
                         colour = "#6495ED",
                         lwd = 1) +
        ggplot2::geom_sf(data = km10_wgs84, # original boundary
                         fill = "transparent",
                         colour = "#0000FF",
                         lwd = 1) +
        ggplot2::labs(x = "Longitude", y = "Latitude", color = "", shape = "", cex.lab = 1)  # legend labels

ggplot2::ggsave(filename = "Basemap.png",
                plot = gg1a,
                width = 5,
                height = 5,
                dpi = 1000)

# Figure 1A
## 2010 population of Ft. Dodge, Iowa by U.S. Census tract
gg1b <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = fd_wgs84, ggplot2::aes(fill = dens), size = 0.1, color = "black") +
        ggplot2::geom_point(data = CAFO, # CAFO location
                            ggplot2::aes(x = lon, y = lat),
                            shape = 13,
                            size = 5,
                            stroke = 1) +
        ggplot2::geom_sf(data = km5, # original boundary
                         fill = "transparent",
                         colour = "#6495ED",
                         lwd = 1) +
        ggplot2::geom_sf(data = km10, # original boundary
                         fill = "transparent",
                         colour = "#0000FF",
                         lwd = 1) +
        ggplot2::scale_fill_gradient(low = "#FFFFFF",
                                     high = "#1A1A1A",
                                     name = "Population Density (per sq km)",
                                     limits = c(0, 2000)) +
        ggplot2::coord_sf(xlim = sf::st_bbox(fd_bbb_wgs84)[c(1, 3)], ylim = sf::st_bbox(fd_bbb_wgs84)[c(2, 4)]) +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = "bottom",
                       legend.title = ggplot2::element_text(size = ggplot2::rel(0.75)))

ggplot2::ggsave(filename = "Figure1A.png",
                plot = gg1b,
                width = 5,
                height = 5,
                dpi = 1000)

# Figure 1B and 2 - IWHS parameters
set.seed(14) # set RNG
## Statistical power
sim_power2 <- sparrpowR::spatial_power(x_case = sf::st_coordinates(utm15)[1], # x-coordinate of case cluster
                                       y_case = sf::st_coordinates(utm15)[2], # y-coordinate of case cluster
                                       x_control = sf::st_coordinates(utm15_FD)[1], # x-coordinate of control cluster
                                       y_control = sf::st_coordinates(utm15_FD)[2], # y-coordinate of control cluster
                                       n_case = 8, # sample size of cases
                                       n_control = 428, # sample size of controls
                                       samp_case = "MVN", # sampler for cases
                                       samp_control = "MVN", # sampler for controls
                                       s_case = 2500/3, # standard deviation of case cluster
                                       s_control = 5000/3, # standard deviation of control cluster
                                       lower_tail = 0.025, upper_tail = 0.975, # two-tailed alpha (defaults)
                                       sim_total = 10000, # number of iterations
                                       parallel = TRUE,
                                       n_core = (parallel::detectCores() - 1),
                                       win = ow) # study area
## Plotting
### Simulated Points
#### If using RStudio, run spatial_plots() and cycle back to first generated plot before adding polygon and point features
spatial_plots(sim_power2, # spatial_power output
              p_thresh = p_thresh, # power threshold (default)
              n_sim = 1, # number of simulations to display (default)
              cols = c("#0000FF", "#00FF00", "#FFFFFF", "#FF0000", "#0000FF"), # colors for plots
              chars = c(16, 16), # symbols for case and control locations 
              sizes = c(1, 0.5)) # size of symbols
plot(km5, add = TRUE, border = "#6495ED", lwd = 3, col = "transparent")
plot(km10, add = TRUE, border = "#0000FF", lwd = 3, col = "transparent")
points(sf::st_coordinates(utm15), col = 1, pch = 13, cex = 3, lwd = 2)

### Power Calculation 
pvalprop <- tibble::tibble(x = sim_power2$rx, y = sim_power2$ry,
                           z = sim_power2$pval_prop) # extract proportion significant
lrr_narm <- na.omit(pvalprop) # remove NAs
sp::coordinates(lrr_narm) <- ~ x + y # coordinates
sp::gridded(lrr_narm) <- TRUE # gridded
pvalprop_raster <- raster::raster(lrr_narm) # convert to raster
raster::crs(pvalprop_raster) <- raster::crs(utm15)
pvalprop_raster <- raster::projectRaster(pvalprop_raster, crs = raster::crs(km10_wgs84)) # unproject (WGS84)
pvalprop_reclass <- raster::reclassify(pvalprop_raster, c(-Inf, p_thresh-0.0000001, 1,
                                                          p_thresh-0.0000001, Inf, 2))
rtp <- raster::rasterToPolygons(pvalprop_reclass) # convert to polygons
rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
rtpFort <- broom::tidy(rtp, data = rtp@data) # convert to tibble
rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data

gg2p <- ggmap::ggmap(fd_base) + # basemap
        ggplot2::geom_polygon(data = rtpFortMer, # output raster as polygons
                              ggplot2::aes(x = long, y = lat, group = group, fill = as.factor(z)), 
                              size = 0, 
                              alpha = 0.5) +
        ggplot2::scale_fill_manual(values = cols[c(5, 2)],
                                   labels = c("insufficient", "sufficient")) + # colors for polygons
        ggplot2::geom_point(data = CAFO, # CAFO location
                            ggplot2::aes(x = lon, y = lat),
                            shape = 13,
                            size = 5,
                            stroke = 1) +
        ggplot2::geom_sf(data = km5, # original boundary
                         fill = "transparent",
                         colour = "#6495ED",
                         lwd = 1) +
        ggplot2::geom_sf(data = km10, # original boundary
                         fill = "transparent",
                         colour = "#0000FF",
                         lwd = 1) +
        ggplot2::labs(x = "Longitude", y = "Latitude", fill = "Power", color = "", shape = "", size = 1) + # legend labels
        ggplot2::theme(text = ggplot2::element_text(size = 10),
                       legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(filename = "Figure2.png",
                plot = gg2p,
                width = 5,
                height = 5,
                dpi = 1000)
rm(pvalprop, lrr_narm, pvalprop_raster, rtpFortMer) # conserve memory

# Figure 3 - IWHS parameters, Increase sample size
set.seed(14) # reset RNG
## Statistical power
sim_power3 <- sparrpowR::spatial_power(x_case = sf::st_coordinates(utm15)[1], # x-coordinate of case cluster
                                       y_case = sf::st_coordinates(utm15)[2], # y-coordinate of case cluster
                                       x_control = sf::st_coordinates(utm15_FD)[1], # x-coordinate of control cluster
                                       y_control = sf::st_coordinates(utm15_FD)[2], # y-coordinate of control cluster
                                       n_case = 18, # sample size of cases
                                       n_control = 982, # sample size of controls
                                       samp_case = "MVN", # sampler for cases
                                       samp_control = "MVN", # sampler for controls
                                       s_case = 2500/3, # standard deviation of case cluster
                                       s_control = 5000/3, # standard deviation of control cluster
                                       cascon = FALSE, # power for case cluster(s) only
                                       lower_tail = 0.025, upper_tail = 0.975, # two-tailed alpha (defaults)
                                       sim_total = 10000, # number of iterations
                                       parallel = TRUE,
                                       n_core = (parallel::detectCores() - 1),
                                       win = ow) # study area
## Plotting
### Simulated Points
#### If using RStudio, run spatial_plots() and cycle back to first generated plot before adding polygon and point features
spatial_plots(sim_power3, # spatial_power output
              p_thresh = 0.8, # power threshold (default)
              n_sim = 1, # number of simulations to display (default)
              cols = c("#0000FF", "#00FF00", "#FFFFFF", "#FF0000", "#0000FF"), # colors for plots
              chars = c(16, 16), # symbols for case and control locations 
              sizes = c(1, 0.5)) # size of symbols
plot(km5, add = TRUE, border = "#6495ED", lwd = 3, col = "transparent")
plot(km10, add = TRUE, border = "#0000FF", lwd = 3, col = "transparent")
points(sf::st_coordinates(utm15), col = 1, pch = 13, cex = 3, lwd = 2)

## Power Calculation 
pvalprop <- tibble::tibble(x = sim_power3$rx, y = sim_power3$ry,
                           z = sim_power3$pval_prop) # extract proportion significant
lrr_narm <- na.omit(pvalprop) # remove NAs
sp::coordinates(lrr_narm) <- ~ x + y # coordinates
sp::gridded(lrr_narm) <- TRUE # gridded
pvalprop_raster <- raster::raster(lrr_narm) # convert to raster
raster::crs(pvalprop_raster) <- raster::crs(utm15)
pvalprop_raster <- raster::projectRaster(pvalprop_raster, crs = raster::crs(km10_wgs84)) # unproject (WGS84)
pvalprop_reclass <- raster::reclassify(pvalprop_raster, c(-Inf, p_thresh-0.0000001, 1,
                                                          p_thresh-0.0000001, Inf, 2))
rtp <- raster::rasterToPolygons(pvalprop_reclass) # convert to polygons
rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
rtpFort <- broom::tidy(rtp, data = rtp@data) # convert to tibble
rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data

gg3p <- ggmap::ggmap(fd_base) + # basemap
        ggplot2::geom_polygon(data = rtpFortMer, # output raster as polygons
                              ggplot2::aes(x = long, y = lat, group = group, fill = as.factor(z)), 
                              size = 0, 
                              alpha = 0.5) +
        ggplot2::scale_fill_manual(values = cols[c(5, 2)],
                                   labels = c("insufficient", "sufficient")) + # colors for polygons
        ggplot2::geom_point(data = CAFO, # CAFO location
                            ggplot2::aes(x = lon, y = lat),
                            shape = 13,
                            size = 5,
                            stroke = 1) +
        ggplot2::geom_sf(data = km5, # original boundary
                         fill = "transparent",
                         colour = "#6495ED",
                         lwd = 1) +
        ggplot2::geom_sf(data = km10, # original boundary
                         fill = "transparent",
                         colour = "#0000FF",
                         lwd = 1) +
        ggplot2::labs(x = "Longitude", y = "Latitude", fill = "Power", color = "", shape = "", size = 1) + # legend labels
        ggplot2::theme(text = ggplot2::element_text(size = 10),
                       legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(filename = "Figure3.png",
                plot = gg3p,
                width = 5,
                height = 5,
                dpi = 1000)
rm(pvalprop, lrr_narm, pvalprop_raster, rtpFortMer) # conserve memory

# Supplemental Figure A
set.seed(14) # reset RNG
## Statistical power
sim_powerSA <- sparrpowR::spatial_power(x_case = sf::st_coordinates(utm15)[1], # x-coordinate of case cluster
                                        y_case = sf::st_coordinates(utm15)[2], # y-coordinate of case cluster
                                        x_control = sf::st_coordinates(utm15_FD)[1], # x-coordinate of control cluster
                                        y_control = sf::st_coordinates(utm15_FD)[2], # y-coordinate of control cluster
                                        n_case = 2, # sample size of cases
                                        n_control = 9998, # sample size of controls
                                        samp_case = "MVN", # sampler for cases
                                        samp_control = "MVN", # sampler for controls
                                        s_case = 2500/3, # standard deviation of case cluster
                                        s_control = 5000/3, # standard deviation of control cluster
                                        cascon = FALSE, # power for case cluster(s) only
                                        lower_tail = 0.025, upper_tail = 0.975, # two-tailed alpha (defaults)
                                        sim_total = 10000, # number of iterations
                                        parallel = TRUE,
                                        n_core = (parallel::detectCores() - 1),
                                        win = ow) # study area
## Plotting
pvalprop <- tibble::tibble(x = sim_powerSA$rx, y = sim_powerSA$ry,
                           z = sim_powerSA$pval_prop) # extract proportion significant
lrr_narm <- na.omit(pvalprop) # remove NAs
sp::coordinates(lrr_narm) <- ~ x + y # coordinates
sp::gridded(lrr_narm) <- TRUE # gridded
pvalprop_raster <- raster::raster(lrr_narm) # convert to raster
raster::crs(pvalprop_raster) <- raster::crs(utm15)
pvalprop_raster <- raster::projectRaster(pvalprop_raster, crs = raster::crs(km10_wgs84)) # unproject (WGS84)
pvalprop_reclass <- raster::reclassify(pvalprop_raster, c(-Inf, p_thresh-0.0000001, 1,
                                                          p_thresh-0.0000001, Inf, 2))
rtp <- raster::rasterToPolygons(pvalprop_reclass) # convert to polygons
rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
rtpFort <- broom::tidy(rtp, data = rtp@data) # convert to tibble
rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data

ggSA <- ggmap::ggmap(fd_base) + # basemap
        ggplot2::geom_polygon(data = rtpFortMer, # output raster as polygons
                              ggplot2::aes(x = long, y = lat, group = group, fill = as.factor(z)), 
                              size = 0, 
                              alpha = 0.5) +
        ggplot2::scale_fill_manual(values = cols[c(5, 2)],
                                   labels = c("insufficient", "sufficient")) + # colors for polygons
        ggplot2::geom_point(data = CAFO, # CAFO location
                            ggplot2::aes(x = lon, y = lat),
                            shape = 13,
                            size = 5,
                            stroke = 1) +
        ggplot2::geom_sf(data = km5, # original boundary
                         fill = "transparent",
                         colour = "#6495ED",
                         lwd = 1) +
        ggplot2::geom_sf(data = km10, # original boundary
                         fill = "transparent",
                         colour = "#0000FF",
                         lwd = 1) +
        ggplot2::labs(x = "Longitude", y = "Latitude", fill = "Power", color = "", shape = "", size = 1) + # legend labels
  ggplot2::theme(text = ggplot2::element_text(size = 10),
                 legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(filename = "FigureSA.png",
                plot = ggSA,
                width = 5,
                height = 5,
                dpi = 1000)
rm(pvalprop, lrr_narm, pvalprop_raster, rtpFortMer) # conserve memory

# Supplemental Figure B
set.seed(14) # reset RNG
## Statistical power
sim_powerSB <- sparrpowR::spatial_power(x_case = sf::st_coordinates(utm15)[1], # x-coordinate of case cluster
                                        y_case = sf::st_coordinates(utm15)[2], # y-coordinate of case cluster
                                        x_control = sf::st_coordinates(utm15_FD)[1], # x-coordinate of control cluster
                                        y_control = sf::st_coordinates(utm15_FD)[2], # y-coordinate of control cluster
                                        n_case = 5, # sample size of cases
                                        n_control = 23995, # sample size of controls
                                        samp_case = "MVN", # sampler for cases
                                        samp_control = "MVN", # sampler for controls
                                        s_case = 2500/3, # standard deviation of case cluster
                                        s_control = 5000/3, # standard deviation of control cluster
                                        cascon = FALSE, # power for case cluster(s) only
                                        lower_tail = 0.025, upper_tail = 0.975, # two-tailed alpha (defaults)
                                        sim_total = 10000, # number of iterations
                                        parallel = TRUE,
                                        n_core = (parallel::detectCores() - 1),
                                        win = ow) # study area
## Plotting
pvalprop <- tibble::tibble(x = sim_powerSB$rx, y = sim_powerSB$ry,
                           z = sim_powerSB$pval_prop) # extract proportion significant
lrr_narm <- na.omit(pvalprop) # remove NAs
sp::coordinates(lrr_narm) <- ~ x + y # coordinates
sp::gridded(lrr_narm) <- TRUE # gridded
pvalprop_raster <- raster::raster(lrr_narm) # convert to raster
raster::crs(pvalprop_raster) <- raster::crs(utm15)
pvalprop_raster <- raster::projectRaster(pvalprop_raster, crs = raster::crs(km10_wgs84)) # unproject (WGS84)
pvalprop_reclass <- raster::reclassify(pvalprop_raster, c(-Inf, p_thresh-0.0000001, 1,
                                                          p_thresh-0.0000001, Inf, 2))
rtp <- raster::rasterToPolygons(pvalprop_reclass) # convert to polygons
rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
rtpFort <- broom::tidy(rtp, data = rtp@data) # convert to tibble
rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data

ggSB <- ggmap::ggmap(fd_base) + # basemap
        ggplot2::geom_polygon(data = rtpFortMer, # output raster as polygons
                              ggplot2::aes(x = long, y = lat, group = group, fill = as.factor(z)), 
                              size = 0, 
                              alpha = 0.5) +
        ggplot2::scale_fill_manual(values = cols[c(5, 2)],
                                   labels = c("insufficient", "sufficient")) + # colors for polygons
        ggplot2::geom_point(data = CAFO, # CAFO location
                            ggplot2::aes(x = lon, y = lat),
                            shape = 13,
                            size = 5,
                            stroke = 1) +
        ggplot2::geom_sf(data = km5, # original boundary
                         fill = "transparent",
                         colour = "#6495ED",
                         lwd = 1) +
        ggplot2::geom_sf(data = km10, # original boundary
                         fill = "transparent",
                         colour = "#0000FF",
                         lwd = 1) +
        ggplot2::labs(x = "Longitude", y = "Latitude", fill = "Power", color = "", shape = "", size = 1) + # legend labels
        ggplot2::theme(text = ggplot2::element_text(size = 10),
                       legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(filename = "FigureSB.png",
                plot = ggSB,
                width = 5,
                height = 5,
                dpi = 1000)
rm(pvalprop, lrr_narm, pvalprop_raster, rtpFortMer) # conserve memory

# Supplemental Figure C
set.seed(14) # reset RNG
## Statistical power
sim_powerSC <- sparrpowR::spatial_power(x_case = sf::st_coordinates(utm15)[1], # x-coordinate of case cluster
                                        y_case = sf::st_coordinates(utm15)[2], # y-coordinate of case cluster
                                        x_control = sf::st_coordinates(utm15_FD)[1], # x-coordinate of control cluster
                                        y_control = sf::st_coordinates(utm15_FD)[2], # y-coordinate of control cluster
                                        n_case = 92, # sample size of cases
                                        n_control = 9908, # sample size of controls
                                        samp_case = "MVN", # sampler for cases
                                        samp_control = "MVN", # sampler for controls
                                        s_case = 2500/3, # standard deviation of case cluster
                                        s_control = 5000/3, # standard deviation of control cluster
                                        cascon = FALSE, # power for case cluster(s) only
                                        lower_tail = 0.025, upper_tail = 0.975, # two-tailed alpha (defaults)
                                        sim_total = 10000, # number of iterations
                                        parallel = TRUE,
                                        n_core = (parallel::detectCores() - 1),
                                        win = ow) # study area
## Plotting
pvalprop <- tibble::tibble(x = sim_powerSC$rx, y = sim_powerSC$ry,
                           z = sim_powerSC$pval_prop) # extract proportion significant
lrr_narm <- na.omit(pvalprop) # remove NAs
sp::coordinates(lrr_narm) <- ~ x + y # coordinates
sp::gridded(lrr_narm) <- TRUE # gridded
pvalprop_raster <- raster::raster(lrr_narm) # convert to raster
raster::crs(pvalprop_raster) <- raster::crs(utm15)
pvalprop_raster <- raster::projectRaster(pvalprop_raster, crs = raster::crs(km10_wgs84)) # unproject (WGS84)
pvalprop_reclass <- raster::reclassify(pvalprop_raster, c(-Inf, p_thresh-0.0000001, 1,
                                                          p_thresh-0.0000001, Inf, 2))
rtp <- raster::rasterToPolygons(pvalprop_reclass) # convert to polygons
rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
rtpFort <- broom::tidy(rtp, data = rtp@data) # convert to tibble
rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data

ggSC <- ggmap::ggmap(fd_base) + # basemap
        ggplot2::geom_polygon(data = rtpFortMer, # output raster as polygons
                              ggplot2::aes(x = long, y = lat, group = group, fill = as.factor(z)), 
                              size = 0, 
                              alpha = 0.5) +
        ggplot2::scale_fill_manual(values = cols[c(5, 2)],
                                   labels = c("insufficient", "sufficient")) + # colors for polygons
        ggplot2::geom_point(data = CAFO, # CAFO location
                            ggplot2::aes(x = lon, y = lat),
                            shape = 13,
                            size = 5,
                            stroke = 1) +
        ggplot2::geom_sf(data = km5, # original boundary
                         fill = "transparent",
                         colour = "#6495ED",
                         lwd = 1) +
        ggplot2::geom_sf(data = km10, # original boundary
                         fill = "transparent",
                         colour = "#0000FF",
                         lwd = 1) +
        ggplot2::labs(x = "Longitude", y = "Latitude", fill = "Power", color = "", shape = "", size = 1) + # legend labels
        ggplot2::theme(text = ggplot2::element_text(size = 10),
                       legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(filename = "FigureSC.png",
                plot = ggSC,
                width = 5,
                height = 5,
                dpi = 1000)
rm(pvalprop, lrr_narm, pvalprop_raster, rtpFortMer) # conserve memory

# Supplemental Figure D
set.seed(14) # reset RNG
## Statistical power
sim_powerSD <- sparrpowR::spatial_power(x_case = sf::st_coordinates(utm15)[1], # x-coordinate of case cluster
                                        y_case = sf::st_coordinates(utm15)[2], # y-coordinate of case cluster
                                        x_control = sf::st_coordinates(utm15_FD)[1], # x-coordinate of control cluster
                                        y_control = sf::st_coordinates(utm15_FD)[2], # y-coordinate of control cluster
                                        n_case = 220, # sample size of cases
                                        n_control = 23780, # sample size of controls
                                        samp_case = "MVN", # sampler for cases
                                        samp_control = "MVN", # sampler for controls
                                        s_case = 2500/3, # standard deviation of case cluster
                                        s_control = 5000/3, # standard deviation of control cluster
                                        cascon = FALSE, # power for case cluster(s) only
                                        lower_tail = 0.025, upper_tail = 0.975, # two-tailed alpha (defaults)
                                        sim_total = 10000, # number of iterations
                                        parallel = TRUE,
                                        n_core = (parallel::detectCores() - 1),
                                        win = ow) # study area
## Plotting
pvalprop <- tibble::tibble(x = sim_powerSD$rx, y = sim_powerSD$ry,
                           z = sim_powerSD$pval_prop) # extract proportion significant
lrr_narm <- na.omit(pvalprop) # remove NAs
sp::coordinates(lrr_narm) <- ~ x + y # coordinates
sp::gridded(lrr_narm) <- TRUE # gridded
pvalprop_raster <- raster::raster(lrr_narm) # convert to raster
raster::crs(pvalprop_raster) <- raster::crs(utm15)
pvalprop_raster <- raster::projectRaster(pvalprop_raster, crs = raster::crs(km10_wgs84)) # unproject (WGS84)
pvalprop_reclass <- raster::reclassify(pvalprop_raster, c(-Inf, p_thresh-0.0000001, 1,
                                                          p_thresh-0.0000001, Inf, 2))
rtp <- raster::rasterToPolygons(pvalprop_reclass) # convert to polygons
rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
rtpFort <- broom::tidy(rtp, data = rtp@data) # convert to tibble
rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data

ggSD <- ggmap::ggmap(fd_base) + # basemap
        ggplot2::geom_polygon(data = rtpFortMer, # output raster as polygons
                              ggplot2::aes(x = long, y = lat, group = group, fill = as.factor(z)), 
                              size = 0, 
                              alpha = 0.5) +
        ggplot2::scale_fill_manual(values = cols[c(5, 2)],
                                   labels = c("insufficient", "sufficient")) + # colors for polygons
        ggplot2::geom_point(data = CAFO, # CAFO location
                            ggplot2::aes(x = lon, y = lat),
                            shape = 13,
                            size = 5,
                            stroke = 1) +
        ggplot2::geom_sf(data = km5, # original boundary
                         fill = "transparent",
                         colour = "#6495ED",
                         lwd = 1) +
        ggplot2::geom_sf(data = km10, # original boundary
                         fill = "transparent",
                         colour = "#0000FF",
                         lwd = 1) +
        ggplot2::labs(x = "Longitude", y = "Latitude", fill = "Power", color = "", shape = "", size = 1) + # legend labels
        ggplot2::theme(text = ggplot2::element_text(size = 10),
                       legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(filename = "FigureSD.png",
                plot = ggSD,
                width = 5,
                height = 5,
                dpi = 1000)
rm(pvalprop, lrr_narm, pvalprop_raster, rtpFortMer) # conserve memory

# Supplemental Figure E
set.seed(14) # reset RNG
## Statistical power
sim_powerSE <- sparrpowR::spatial_power(x_case = sf::st_coordinates(utm15)[1], # x-coordinate of case cluster
                                        y_case = sf::st_coordinates(utm15)[2], # y-coordinate of case cluster
                                        x_control = sf::st_coordinates(utm15_FD)[1], # x-coordinate of control cluster
                                        y_control = sf::st_coordinates(utm15_FD)[2], # y-coordinate of control cluster
                                        n_case = 183, # sample size of cases
                                        n_control = 9817, # sample size of controls
                                        samp_case = "MVN", # sampler for cases
                                        samp_control = "MVN", # sampler for controls
                                        s_case = 2500/3, # standard deviation of case cluster
                                        s_control = 5000/3, # standard deviation of control cluster
                                        cascon = FALSE, # power for case cluster(s) only
                                        lower_tail = 0.025, upper_tail = 0.975, # two-tailed alpha (defaults)
                                        sim_total = 10000, # number of iterations
                                        parallel = TRUE,
                                        n_core = (parallel::detectCores() - 1),
                                        win = ow) # study area
## Plotting
pvalprop <- tibble::tibble(x = sim_powerSE$rx, y = sim_powerSE$ry,
                           z = sim_powerSE$pval_prop) # extract proportion significant
lrr_narm <- na.omit(pvalprop) # remove NAs
sp::coordinates(lrr_narm) <- ~ x + y # coordinates
sp::gridded(lrr_narm) <- TRUE # gridded
pvalprop_raster <- raster::raster(lrr_narm) # convert to raster
raster::crs(pvalprop_raster) <- raster::crs(utm15)
pvalprop_raster <- raster::projectRaster(pvalprop_raster, crs = raster::crs(km10_wgs84)) # unproject (WGS84)
pvalprop_reclass <- raster::reclassify(pvalprop_raster, c(-Inf, p_thresh-0.0000001, 1,
                                                          p_thresh-0.0000001, Inf, 2))
rtp <- raster::rasterToPolygons(pvalprop_reclass) # convert to polygons
rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
rtpFort <- broom::tidy(rtp, data = rtp@data) # convert to tibble
rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data

ggSE <- ggmap::ggmap(fd_base) + # basemap
        ggplot2::geom_polygon(data = rtpFortMer, # output raster as polygons
                              ggplot2::aes(x = long, y = lat, group = group, fill = as.factor(z)), 
                              size = 0, 
                              alpha = 0.5) +
        ggplot2::scale_fill_manual(values = cols[c(5, 2)],
                                   labels = c("insufficient", "sufficient")) + # colors for polygons
        ggplot2::geom_point(data = CAFO, # CAFO location
                            ggplot2::aes(x = lon, y = lat),
                            shape = 13,
                            size = 5,
                            stroke = 1) +
        ggplot2::geom_sf(data = km5, # original boundary
                         fill = "transparent",
                         colour = "#6495ED",
                         lwd = 1) +
        ggplot2::geom_sf(data = km10, # original boundary
                         fill = "transparent",
                         colour = "#0000FF",
                         lwd = 1) +
        ggplot2::labs(x = "Longitude", y = "Latitude", fill = "Power", color = "", shape = "", size = 1) + # legend labels
        ggplot2::theme(text = ggplot2::element_text(size = 10),
                       legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(filename = "FigureSE.png",
                plot = ggSE,
                width = 5,
                height = 5,
                dpi = 1000)
rm(pvalprop, lrr_narm, pvalprop_raster, rtpFortMer) # conserve memory

# Supplemental Figure F
set.seed(14) # reset RNG
## Statistical power
sim_powerSF <- sparrpowR::spatial_power(x_case = sf::st_coordinates(utm15)[1], # x-coordinate of case cluster
                                        y_case = sf::st_coordinates(utm15)[2], # y-coordinate of case cluster
                                        x_control = sf::st_coordinates(utm15_FD)[1], # x-coordinate of control cluster
                                        y_control = sf::st_coordinates(utm15_FD)[2], # y-coordinate of control cluster
                                        n_case = 440, # sample size of cases
                                        n_control = 23560, # sample size of controls
                                        samp_case = "MVN", # sampler for cases
                                        samp_control = "MVN", # sampler for controls
                                        s_case = 2500/3, # standard deviation of case cluster
                                        s_control = 5000/3, # standard deviation of control cluster
                                        cascon = FALSE, # power for case cluster(s) only
                                        lower_tail = 0.025, upper_tail = 0.975, # two-tailed alpha (defaults)
                                        sim_total = 10000, # number of iterations
                                        parallel = TRUE,
                                        n_core = (parallel::detectCores() - 1),
                                        win = ow) # study area

## Plotting
pvalprop <- tibble::tibble(x = sim_powerSF$rx, y = sim_powerSF$ry,
                           z = sim_powerSF$pval_prop) # extract proportion significant
lrr_narm <- na.omit(pvalprop) # remove NAs
sp::coordinates(lrr_narm) <- ~ x + y # coordinates
sp::gridded(lrr_narm) <- TRUE # gridded
pvalprop_raster <- raster::raster(lrr_narm) # convert to raster
raster::crs(pvalprop_raster) <- raster::crs(utm15)
pvalprop_raster <- raster::projectRaster(pvalprop_raster, crs = raster::crs(km10_wgs84)) # unproject (WGS84)
pvalprop_reclass <- raster::reclassify(pvalprop_raster, c(-Inf, p_thresh-0.0000001, 1,
                                                          p_thresh-0.0000001, Inf, 2))
rtp <- raster::rasterToPolygons(pvalprop_reclass) # convert to polygons
rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
rtpFort <- broom::tidy(rtp, data = rtp@data) # convert to tibble
rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data

ggSF <- ggmap::ggmap(fd_base) + # basemap
        ggplot2::geom_polygon(data = rtpFortMer, # output raster as polygons
                              ggplot2::aes(x = long, y = lat, group = group, fill = as.factor(z)), 
                              size = 0, 
                              alpha = 0.5) +
        ggplot2::scale_fill_manual(values = cols[c(5, 2)],
                                   labels = c("insufficient", "sufficient")) + # colors for polygons
        ggplot2::geom_point(data = CAFO, # CAFO location
                            ggplot2::aes(x = lon, y = lat),
                            shape = 13,
                            size = 5,
                            stroke = 1) +
        ggplot2::geom_sf(data = km5, # original boundary
                         fill = "transparent",
                         colour = "#6495ED",
                         lwd = 1) +
        ggplot2::geom_sf(data = km10, # original boundary
                         fill = "transparent",
                         colour = "#0000FF",
                         lwd = 1) +
        ggplot2::labs(x = "Longitude", y = "Latitude", fill = "Power", color = "", shape = "", size = 1) + # legend labels
        ggplot2::theme(text = ggplot2::element_text(size = 10),
                       legend.text = ggplot2::element_text(size = 8))

ggplot2::ggsave(filename = "FigureSF.png",
                plot = ggSF,
                width = 5,
                height = 5,
                dpi = 1000)
rm(pvalprop, lrr_narm, pvalprop_raster, rtpFortMer) # conserve memory

### Supplemental Plot (multi-plot)
gg_inset_map <- cowplot::ggdraw() +
                cowplot::draw_label("(A)", x = 0.225, y = 0.98, size = 20) +
                cowplot::draw_label("(B)", x = 0.725, y = 0.98, size = 20) +
                cowplot::draw_label("(C)", x = 0.225, y = 0.655, size = 20) +
                cowplot::draw_label("(D)", x = 0.725, y = 0.655, size = 20) +
                cowplot::draw_label("(E)", x = 0.225, y = 0.33, size = 20) +
                cowplot::draw_label("(F)", x = 0.725, y = 0.33, size = 20) +
                cowplot::draw_plot(ggSA, x = 0.0, y = 0.575, width = 0.5, height = 0.5) +
                cowplot::draw_plot(ggSB, x = 0.5, y = 0.575, width = 0.5, height = 0.5) +
                cowplot::draw_plot(ggSC, x = 0.0, y = 0.25, width = 0.5, height = 0.5) +
                cowplot::draw_plot(ggSD, x = 0.5, y = 0.25, width = 0.5, height = 0.5) +
                cowplot::draw_plot(ggSE, x = 0.0, y = -0.075, width = 0.5, height = 0.5) +
                cowplot::draw_plot(ggSF, x = 0.5, y = -0.075, width = 0.5, height = 0.5)

f <- 2 # graphical expansion
ggplot2::ggsave(filename = "supplemental.png",
                plot = gg_inset_map,
                width = 6*f,
                height = 8*f,
                dpi = 1000)
# ----------------------- END OF CODE ----------------------- #
