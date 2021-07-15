## This is the seventh resubmission

* Actions taken since previous submission:
  * Following advice from `future` package maintainer, now `spatial_power()` and `jitter_power()` functions reset future strategy when exiting
  * Addressed ERROR in MacOS CRAN environments by setting the `parallelly.makeNodePSOCK.setup_strategy = sequential` for MacOS environments running `tcltk` until `parallelly` (>=1.26.1-9002) is on CRAN. This workaround was suggested by the `parallelly` maintainer <https://github.com/HenrikBengtsson/parallelly/issues/62#issuecomment-880665390>

* Documentation for `pval_correct()` references a doi <https://doi.org/10.2307/2283989> that throws a NOTE but is a valid URL
  
## Test environments
* local OS X install, R 4.1.0
* win-builder, (devel, release, oldrelease)
* Rhub
  * Fedora Linux, R-devel, clang, gfortran
  * Ubuntu Linux 16.04 LTS, R-release, GCC
  * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  * Oracle Solaris 10, x86, 32 bit, R-release
  * macOS 10.13.6 High Sierra, R-release, CRAN's setup

## R CMD check results
0 errors | 0 warnings | 0 notes

## Submitted by Maintainer