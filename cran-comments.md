## This is the thirteenth resubmission

* Actions taken since previous submission:
  * Renamed 'package.R' to 'sparrpowR-package.R' after Roxygen (>=7.3.0) update
  * Fixed Rd `\link{}` targets missing package within 'sparrpowR-package.Rd', 'spatial_data.Rd', and 'spatial_power.Rd'
  * Updated CITATION

* Documentation for `pval_correct()` references a doi <https://doi.org/10.2307/2283989> that throws a NOTE but is a valid URL
  
## Test environments
* local Windows install, R 4.5.1
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
