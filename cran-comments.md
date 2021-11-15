
# Version 0.1.5

## Round 1

### Test environments
- local: Ubuntu 20.04, R 4.1.2
- win builder: R release and devel

### R CMD check results

There were no errors or warnings. There was one note. 

1. That the package was archived.

### What changed
- Removed dependency on the `bcpa` package.
- Fixed test errors (Email by B. Ripley on 2021-10-26)
- support for `hnorm` and `lnorm` step-length distributions.
- support for `st_crs` for CRS. Major updates for several functions
- deprecated the function `dist_cent`.
- Added `as_track` for `data.frames`.
- Added argument `verbose` to `make_track()`. 
- Fixed bug with `hr_isopleths()` for aKDE.
- Fix issue #48 (https://github.com/jmsigner/amt/issues/48). Zero step lengths return now NA for direction.


# Version 0.1.4

## Round 1

### Test environments
- local: Ubuntu 19.10, R 4.0.0
- win builder: R release and devel
- travis-ci: R release and devel
- appveyor
- macos-highsierra-release-cran on rhub

### R CMD check results

There were no errors, warnings or notes. 

### What changed
- `unnest` works for track_xy*
- Major updates for `hr_akde`. This includes that now CI are available for  
- new vignette and methods for `hr_overlap`
- reduced package dependencies.
- Fixed bug in confidence calculation for `log_rss`.

# Version 0.1.3

## Round 1

### Test environments
- local: Ubuntu 19.10, R 4.0.0
- win builder: release and devel
- travis-ci: release and devel
- appveyor
- macos-highsierra-release-cran on rhub

### R CMD check results

There were no errors or warnings. On some platforms I received a note indicating that I have to many imports from non-default packages (local check).

### What changed

- `hr_overlap` gained two new arguments `labels` and `consecutive.only`.
- `log_rss` new CI
- I was asked to move `Rdpack` to imports. 
- `group_by` for random points works as expected.


# Version 0.1.2

## Round 1

### Test environments
- local: Ubuntu 19.10, R 4.0.0
- win builder: release and devel
- travis-ci: release 
- appveyor
- macos-highsierra-release-cran on rhub

### R CMD check results

There were no errors or warnings. On some platforms I received a note indicating that I have to many imports from non-default packages (local check).

### What changed
- `hr_mcp` gained an argument `keep.data`
- `as_track` can now convert `steps_xyt` back to a `track_xyt`
- `plot` function for home ranges improved
- Ported function for `site_fidelity` from package `rhr`.
- Changed test in order to be compatible with the new dplyr version. 





# Version 0.1.1

## Round 1

### Test environments
- local: Ubuntu 18.04.2 LTS, R 3.6.3
- win builder: release (devel was not available)
- travis-ci: release and devel
- appveyor
- macos-highsierra-release-cran on rhub

### R CMD check results

There were no errors or warnings. On some platforms I received a note indicating that I have to many imports from non-default packages. I will reduce the number of imports in futur versions of the `amt` package.

### What changed
#### bug fixes
- Fixed issues for CRAN submission of version 0.1.0, in particular imports and tests.



# Version 0.1.0
## Round 2

The unit tests gave an error for MacOS. I was asked by Prof. B. Ripley to resolve these. 

I this and tested the package again with

```
rhub::check(platform = "macos-highsierra-release-cran")
```

There were no errors warnings or notes.

## Round 1

### Test environments
- local: Ubuntu 18.04.2 LTS, R 3.6.3
- win builder: release and devel
- travis-ci: release and devel
- appveyor

### R CMD check results

There were no errors, warnings or notes

### What changed
#### bug fixes
- `random_steps` uses abs direction as reference instead of relative direction

#### updates
- Streamlined home-range methods
- Added methods for overlaps and intersections of home ranges
- Added href scaled for KDE


# Version 0.0.9.0

## Round 2

I was asked to fix the references in the Description to `<doi:prefix/suffix>` which is now done. 


### Test environments
- local: Ubuntu 18.04.2 LTS, R 3.6.3
- win builder: release and devel
- travis-ci: release and devel
- appveyor

There were no errors, warnings or notes.

## Round 1

### Test environments
- local: Ubuntu 18.04.2 LTS, R 3.6.3
- win builder: release and devel
- travis-ci: release and devel
- appveyor

### What changed
#### bug fixes
- `extract_covariate` now works with new version of tibble for raster stacks.
- `fit_distr` for gamma distribution, now replaces 0 step length with min setep- length and informs the user with a message. 
#### minor changes
- added citation to the description
- unnamed elements of fitted gamma

#### major changes
- replaced `testthat` with `tinytest`

### R CMD check results

There were no errors or warnings. There was 1 note.

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Johannes Signer <jsigner@gwdg.de>'

Days since last update: 4

The Description field contains
  <https://doi.org/10.1890/04-0953> and integrated step-selection
  functions <https://doi.org/10.1111/2041-210X.12528>), and simulation of
  <https://doi.org/10.1002/ecs2.1771>.```
```


# Version 0.0.8.0
## Round 1

### Test envionments
- local: Ubuntu 18.04.2 LTS, R 3.6.3
- win builder: release and devel
- travis-ci: release and devel
- appveyor

### What changed

#### new features
- New vignette for RSFs.
- New functions to calculate RSS.
- New vignette for interfacing other packages.
- Added method to interface the `move` packages.

#### major changes
- Improved `random_points` by addeing methods for track, hr and spatial objects.
- Adjusted `random_steps`
- Removed dependency on `velox`. This makes `extract_covariates_along` slower. 

### R CMD check results

There were no errors or warnings. There was 1 note.

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Johannes Signer <jsigner@gwdg.de>'

New submission

Package was archived on CRAN

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2020-03-17 as depends on archived package
    'velox'.
```



# Version 0.0.7.0
## Round 1

### Test envionments
- local: Ubuntu 18.04.2 LTS, R 3.6.1
- win builder
- travis-ci: release and devel
- appveyor

### What changed

- I was requested to fix warnings, error that were introduced by updates in the tidyr package. 

### R CMD check results

There were no errors or warnings.

There was one note using win builder: 

```
* checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
Maintainer: 'Johannes Signer <jsigner@gwdg.de>'
```

# Version 0.0.6.0
## Round 1
## Test environments
* local Ubuntu 18.04.2 LTS, R 3.5.2
* win-builder (devel and release)
* travis-ci: release and devel
* appveyor
* r-hub: fedora-clang-devel

### What changed
- I was requested to address a warning message (https://cran.r-project.org/web/checks/check_results_amt.html). Which is now fixed.
- Added new methods to estimate SSF with temporally varying covariates.

## minor changes
- Several typos are fixed
- Updated dependencies to dplyr.


### R CMD check results
There were no ERRORs or WARNINGs.

- There was one NOTE on the `fedora-clang-devel` plattform:

```
* checking dependencies in R code ... NOTE
Namespaces in Imports field not imported from:
  ‘Rcpp’ ‘magrittr’
  All declared Imports should be used.
```

Both packages (`Rcpp` and `magrittr`) are used. I export the pipe operator (`%>%`) from `magrittr` and use `Rcpp` for source compiled code.


## Downstream dependencies
There are no downstream dependencies.


----

# Version 0.0.5.0
# Round 1
## Test environments
* local Ubuntu 16.04.3 LTS, R 3.4.4
* win-builder (devel and release)

### What changed
- `random_steps` gained a new distribution for random steps (exponential).
- bug fix in `random_steps`
- new function `extract_covariates_along`, extracts covariates along a random step.
- `steps` gains a new argument: `keep_cols` in order to keep columns from point when creating steps.
- `simulate_tud`: a convenience wrapper arround `simulate_ud` to simulate transition UDs.
- Added an alias to `mk_track` called `make_track`.
- `mk_track` gained a new argument `check_duplicates` that allows to the user to specify if temporal duplicates should give an error or not.
- Added `hr_kde` to calculate Kernel home ranges.
- `steps` now uses `difftime` to calcualte time differences.

## minor changes
- Removed: Depends tidyverse
- Fixed errors with lubridate::Periods within `data_frame`s


### R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 


## Downstream dependencies
There are no downstream dependencies.

---

# Version 0.0.3.0
# Round 1
## Test environments
* local Ubuntu 16.04.3 LTS, R 3.4.3
* win-builder (devel and release)

### What changed

Minor update to fix:
- Vignette titles
- Add `Rdpack` (used for Rdmacros) to suggested packages.

### R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 


## Downstream dependencies
There are no downstream dependencies.

---

# Version 0.0.2.0
# Round 1
## Test environments
* local Ubuntu 16.04.3 LTS, R 3.4.3
* win-builder (devel and release)

### R CMD check results
There were no ERRORs or WARNINGs. 

There was one NOTE:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Johannes Signer <jsigner@gwdg.de>'
```

New submission

## Downstream dependencies
There are no downstream dependencies.

## Reviewer comments

Thanks, please write package names and software names in single quotes
(e.g. 'amt') in title and description.

Please fix and resubmit.


# Round 2

## Test environments
* local Ubuntu 16.04.3 LTS, R 3.4.3
* win-builder (devel and release)

### R CMD check results
There were no ERRORs or WARNINGs. 

There was one NOTE:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Johannes Signer <jsigner@gwdg.de>'
```

## Response to requests
I adjusted the `Description` of the package and enclosed amt (the package name) in single quotes.

## Downstream dependencies
There are no downstream dependencies.


