## This is the thirteenth resubmission

* Actions taken since previous submission:
  * Renamed 'package.R' to 'sparrpowR-package.R' after Roxygen (>=7.3.0) update
  * Fixed Rd `\link{}` targets missing package within 'sparrpowR-package.Rd', 'spatial_data.Rd', and 'spatial_power.Rd'
  * Updated CITATION

* Documentation for `pval_correct()` references a doi <https://doi.org/10.2307/2283989> that throws a NOTE but is a valid URL
* Two links in NEWS.md throw a NOTE but are valid URLs:
  * <https://github.com/HenrikBengtsson/parallelly/issues/62#issuecomment-880665390>
  * <https://github.com/HenrikBengtsson/parallelly/issues/65>
  
## Test environments
* local Windows install, R 4.5.1
* win-builder (devel, release, oldrelease)
* R-CMD-check on GitHub
  * macos-latest (release)
  * windows-latest (release)
  * ubuntu-latest (devel)
  * ubuntu-latest (release)
  * ubuntu-latest (oldrel-1)
* Rhub v2
  * macos-15 on GitHub, ASAN + UBSAN on macOS (`m1-san`)
  * macos-13 on GitHub(`macos`)
  * Fedora Linux 40 (Container Image) (`gcc-asan`)
  * Ubuntu 22.04.5 LTS (`ubuntu-clang`)
  * Ubuntu 22.04.5 LTS (`ubuntu-gcc12`)

## R CMD check results
0 errors | 0 warnings | 0 notes

## Submitted by Maintainer
