# authorship: https://journal.r-project.org/archive/2012-1/RJournal_2012-1_Hornik~et~al.pdf

install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
library(devtools)
devtools::has_devel()
library(roxygen2)
library(testthat)

devtools::load_all()

.onload <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "envi",
    devtools.desc.author = person(given = "Ian D.",
                                  family = "Buller",
                                  role = c("aut", "cre", "cph"),
                                  email = "ian.buller@nih.gov"),
    devtools.desc.license = "Apache License (>= 2.0)",
    devtools.desc.suggest = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])
  
  invisible()
}

getOption("sparrpowR")

# Convert roxygen components to .Rd files
devtools::document()

# Create Vignette
devtools::install()
devtools::build()

# Testing
devtools::use_testthat()
devtools::use_test()
devtools::test()

# NAMESPACE
document()
install()

# Check
devtools::check()

# Ignore .R files from /dev directory
usethis::use_build_ignore(c("dev"))

# Check on windows
devtools::check_win_devel()
devtools::check_win_oldrelease()
devtools::check_win_release()

# rhub
devtools::check_rhub()
rhub::check_for_cran()

# Release to CRAN
#devtools::release()

# Example in README
library(sparrpowR)
library(stats)
set.seed(1234)

# ----------------- #
# Run spatial_power #
# ----------------- #

# Circular window with radius 0.5
# Uniform case sampling within a disc of radius of 0.1 at the center of the window
# Complete Spatial Randomness control sampling
# 20% prevalence (n = 300 total locations)
# Statistical power to detect both case and control relative clustering
# 100 simulations (more recommended for power calculation)

unit.circle <- spatstat::disc(radius = 0.5, centre = c(0.5,0.5))

foo <- spatial_power(win = unit.circle,
                     sim_total = 100,
                     x_case = 0.5,
                     y_case = 0.5,
                     samp_case = "uniform",
                     samp_control = "CSR",
                     r_case = 0.1,
                     n_case = 50,
                     n_control = 250)

# ----------------------- #
# Outputs from iterations #
# ----------------------- #

# Mean and standard deviation of simulated sample sizes and bandwidth
mean(foo$n_con); stats::sd(foo$n_con)    # controls
mean(foo$n_cas); stats::sd(foo$n_cas)    # cases
mean(foo$bandw); stats::sd(foo$bandw)    # bandwidth of case density (if fixed, same for control density) 

# Global Test Statistics
## Global maximum relative risk: Null hypothesis is mu = 1
stats::t.test(x = foo$s_obs, mu = 0, alternative = "two.sided")

## Integral of log relative risk: Null hypothesis is mu = 0
stats::t.test(x = foo$t_obs, mu = 1, alternative = "two.sided")

# ----------------- #
# Run spatial_plots #
# ----------------- #

spatial_plots(foo,
              p_thresh = 0.9,
              chars = c(4,5),
              sizes = c(0.6,0.3),
              cols = c("blue", "green", "red", "purple", "orange"),
              plot_axes = TRUE,
              cascon = TRUE,
              horizontal = FALSE)

set.seed(1234)
f1 <- jitter_power(obs_data = unique(chorley),
                   sim_total = 100,
                   samp_control = "CSR",
                   resolution = 20,
                   p_correct = "FDR",
                   verbose = FALSE)
spatial_plots(f1)
median(f1$alpha_correct)
