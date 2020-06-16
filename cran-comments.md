## This is the third submission
* Actions taken regarding message from Prof Brian Ripley to be corrected before 2020-06-30 that "Package V8 no longer installs on fedora-clang after upgrading to Fedora 32, suggest it or a package which requires it but do not use it conditionally."
  * Changed vignette to static html
  * Removed packages from Suggests not required anymore. 
  
## Test environments
* local OS X install, R 3.6.2
* win-builder, (devel, release)
* Rhub
  * Oracle Solaris 10, x86, 32 bit, R-release
  * Fedora Linux, R-devel, GCC
  * Debian Linux, R-devel, GCC
  * Fedora Linux, R-devel, clang, gfortran

## R CMD check results
0 errors | 0 warningss | 0 notes

## Submitted by contributor/co-author