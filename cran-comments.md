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


