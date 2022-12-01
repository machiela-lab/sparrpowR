## This is the tenth resubmission

* Actions taken since previous submission:
  * Replaced `if()` conditions comparing `class()` to string with `inherits()`
  * Added `future::plan(future::multisession)` in tests to remove the files in temp directory
  * Updated maintainer contact information

* Documentation for `pval_correct()` references a doi <https://doi.org/10.2307/2283989> that throws a NOTE but is a valid URL
  
## Test environments
* local OS X install, R 4.2.1
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