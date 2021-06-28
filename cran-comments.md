## This is the sixth resubmission

* Actions taken since previous submission:
  * Removed `LazyData: true` from 'DESCRIPTION' file because the package has no data accessed via a `data()` command and has no `data/` directory (in response to CRAN NOTE: 'LazyData' is specified without a 'data' directory)
  * Changed hyperlink for Stamen basemap in 'vignette' file with a Hypertext Transfer Protocol Secure destination
  * Added 'CITATION' file
  * Deprecation badges in 'jitter_power.Rd' and 'spatial_power.Rd' files now hyperlink to `lifecycle` r-lib.org site

* Documentation for `pval_correct()` references a doi <https://doi.org/10.2307/2283989> that throws a NOTE in win-builder but no other environment
  
## Test environments
* local OS X install, R 4.0.4
* win-builder, (devel, release, oldrelease)
* Rhub
  * Fedora Linux, R-devel, clang, gfortran
  * Ubuntu Linux 16.04 LTS, R-release, GCC
  * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  * Oracle Solaris 10, x86, 32 bit, R-release

## R CMD check results
0 errors | 0 warnings | 0 notes

## Submitted by Maintainer