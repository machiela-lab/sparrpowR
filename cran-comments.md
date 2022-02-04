## This is the ninth resubmission

* Actions taken since previous submission:
  * Updated dependencies `spatstat.core` and `spatstat.linnet` packages based on feedback from the Spatstat Team (Adrian Baddeley and Ege Rubak). All random generators in `spatstat.core` were moved to a new package `spatstat.random`
    * `spatstat.geom`, `spatstat.core`, `spatstat.linnet`, and `spatstat (>=2.0-0)` are no longer Depends
    * `spatstat.geom` and `spatstat.random` are now Imports
    * `spatstat.data` is now Suggests
    * [See the GitHub merge pull request](https://github.com/machiela-lab/sparrpowR/commit/4df5d85343dd222c9d4b1ae30f894ed6482bcb52)
  * Fixed annotation typos in the `pval_correct()` function

* Documentation for `pval_correct()` references a doi <https://doi.org/10.2307/2283989> that throws a NOTE but is a valid URL
  
## Test environments
* local OS X install, R 4.1.2
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