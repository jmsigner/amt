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


