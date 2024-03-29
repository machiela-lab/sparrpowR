# ----------------------------------------------------- #
# HexSticker for the "sparrpowR" package
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: June 4, 2020
#
# Recently modified by: @idblr
# Recently modified on: February 4, 2022
#
# Notes:
# A) Uses the "hexSticker" pacakge
# B) Option 1: Triforce
# C) Various color palettes
# D) Option 2: Orion's Sword
# E) 2022/04/02: Updated 'spatstat' suite packages
# ----------------------------------------------------- #

# Option 1: Triforce

# Packages
library(spatstat.geom)
library(spatstat.random)
library(hexSticker)

# Settings
set.seed(1234)
par(pty = "s")

# Create windows for separate point patterns
angle <- pi/3
x <- c(0,0.5,0.5*cos(angle),0)+1
y <- c(0,0,0.5*sin(angle),0)+1
y1 <- c(0,0,0.5*-sin(angle),0)+1
x2 <- c(1-0.33, 1.1, 1.25, 1.6, 1.5+0.33, 1-0.33)
y2 <- c(1.5, 1, 1.433013, 1, 1.5, 1.5)

# Create point patterns
g1 <- rsyst(nx = 20, win = spatstat.geom::owin(poly=list(x=x,y=y)))
g2 <- rsyst(nx = 20, win = spatstat.geom::owin(poly=list(x=rev(x),y=rev(y1))))
g3 <- spatstat.random::rpoispp(lambda = 20, win = spatstat.geom::owin(poly=list(x=x2,y=y2)))

# Set marks for point patterns (for colors)
spatstat.geom::marks(g1) <- 1
spatstat.geom::marks(g2) <- 2
spatstat.geom::marks(g3) <- 1

# Combine point patterns and set marks to be factors
g4 <- spatstat.geom::superimpose(g1, g2, g3)
spatstat.geom::marks(g4) <- as.factor(spatstat.geom::marks(g4))
#spatstat.geom::plot.ppp(g4, cols = c("moccasin", "sienna1"), show.all = F)

# Create hexSticker
## the Balance Within palette https://www.color-hex.com/color-palette/14625
s <- hexSticker::sticker(~ spatstat.geom::plot.ppp(g4, cols = c("#ee9a00", "#698b69"),
                                                   show.all = F, cex = 0.5,
                                                   pch = c(8,8)),
                         package = "sparrpowR", p_size = 5, p_color = "#ffe4b5",
                         s_x = 0.9, s_y = 0.35, s_width = 3.33, s_height = 3.33,
                         h_fill = "#344960",
                         h_color = "#8b3a3a",
                         dpi = 1000,
                         filename = "./man/figures/sparrpowR.png",
                         white_around_sticker = T)

# -------------------- END OF CODE -------------------- #
