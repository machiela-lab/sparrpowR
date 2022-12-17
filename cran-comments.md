## This is the eleventh resubmission

* Actions taken since previous submission:
  * Migrated R-spatial dependency
  * Replaced `raster` and `sp` in Imports with `terra` because of imminent package retirement
  * `geojsonsf`, `ggmap`, `ggplot2`, `sf`, `tidyterra` are now Suggests because they are used in the vignette
  * Updated vignette and documentation throughout

* Documentation for `pval_correct()` references a doi <https://doi.org/10.2307/2283989> that throws a NOTE but is a valid URL
  
## Test environments
* local Windows install, R 4.2.1
* win-builder, (devel, release, oldrelease)
* Rhub
  * Fedora Linux, R-devel, clang, gfortran
  * Ubuntu Linux 20.04.1 LTS, R-release, GCC
  * Windows Server 2022, R-devel, 64 bit
  * Windows Server 2008 R2 SP1, R-release, 32/64 bit
  * Oracle Solaris 10, x86, 32 bit, R-release
  * macOS 10.13.6 High Sierra, R-release, CRAN's setup

## R CMD check results
0 errors | 0 warnings | 0 notes

## Submitted by Maintainer
