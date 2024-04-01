# amt 0.3.0

# amt 0.2.2
## Fixes
- Fixed error within `random_steps.brusted_track_xyt()`, when `burst_id` did not start with 1. 
- Removed `as_move()`. 
- Fixed documentation (requested by CRAN)
- Fixed issue in `make_trast()` (reported by joshcullen; #105)
- Fixed issue `lonlat=TRUE` when calculating step lengths (#103, #107, #108)

# amt 0.2.1
## Fixes
- The correct step id is now given in `random_steps.brusted_track_xyt()`.

# amt 0.2
## updates
- Removed dependency on Rccp, all code is now in R. 
- `extract_covariates()` no longer accepts a buffer. 
- Updated `time_of_day()` to use `suncalc` instead of `maptools`.
- Updated `hr_overlap()` from `raster` to `terra`. 
- Updated `od()` and `hr_od()` from `raster` to `terra`. 
- Updated `hr_isopleths()` from `raster`/`sp` to `terra`/`sf`. 
- `hr_cud()` updated from `raster` to `terra`. 
- `extrackt_covariates*()` updated from `raster` to `terra`.
- `distance_to_center()` was removed. 
- Updated `step`, it now uses `sf::st_distance()` instead of `raster::distance()`. 
- Dependency on `raster`, `sp`, `rgeos` and `spdep` were removed. 
- `as_sp()` is now `as_sf()` and replaced the function call in the whole package.
- Rewrite parts of `hr_akde()` to use `terra` instead of `raster`.
- Rewrite parts of `hr_locoh()` to use `sf` instead of `rgeos`.

## Fixes
- Fixed error in `ta_correlation()` reported by Ines Khazar. 

# amt 0.1.8
## updates
- `fit_clogit()` checks for strata.

# amt 0.1.7
## updates
- Accepted merge to have to have buffers for `extract_covariates()`.
- Accepted merge to fix error in `as_ltraj()`.
- Accepted merge to typo. 
- New method `remove_incomplete_strata()`


# amt 0.1.6
## updates
- fixed issue #54.
- fixed issue #43 by adding a an example. 
- fixed issue #57.
- fixed issue #58.
- fixed issue #56: levels for `akde`
- added enhancement #55
- fixed small error in `steps()` for calculating the absolute direction. 
- New method `add_nsd()` to calculate the NSD for tracks and steps. 
- Fixed error with time_of_day and CRS

# amt 0.1.5
## updates
- support for `hnorm` and `lnorm` step-length distributions.
- support for `st_crs` for CRS. Major updates for several functions
- deprecated the function `dist_cent`.
- Added `as_track` for `data.frames`.
- Added argument `verbose` to `make_track()`. 
- Fixed bug with `hr_isopleths()` for aKDE.
- Fix issue #48 (https://github.com/jmsigner/amt/issues/48). Zero step lengths return now NA for direction.

# amt 0.1.4
## updates
- `unnest` works for track_xy*
- Major updates for `hr_akde`. This includes that now CI are available for  
- new vignette and methods for `hr_overlap`
- reduced package dependencies.

## bug fixes
- Fixed bug in confidence calculation for `log_rss`.


# amt 0.1.3
## updates
- `hr_overlap` gained two new arguments `labels` and `consecutive.only`.
- `log_rss` new CI
- I was asked to move `Rdpack` to imports. 

## bug fixes
- `group_by` for random points works as expected.


# amt 0.1.2
## bug fixes

## updates
- `hr_mcp` gained an argument `keep.data`
- `as_track` can now convert `steps_xyt` back to a `track_xyt`
- `plot` function for home ranges improved
- Ported function for `site_fidelity` from package `rhr`.
- Changed test in order to be compatible with the new dplyr version. 


# amt 0.1.0
## bug fixes
- `random_steps` uses abs direction as reference instead of relative direction

## updates
- Streamlined home-range methods
- Added methods for overlaps and intersections of home ranges
- Added href scaled for KDE


# amt 0.0.9
## bug fixes
- `extract_covariate` now works with new version of tibble for raster stacks.
- `fit_distr` for gamma distribution, now replaces 0 step length with min step- length and informs the user with a message. 

## minor changes
- added citation to the description
- unnamed elements of fitted gamma

## major changes
- replaced `testthat` with `tinytest`


# amt 0.0.8
## new features
- New vignette for RSFs.
- New functions to calculate RSS.
- New vignette for interfacing other packages.
- Added method to interface the `move` packages.


## major changes
- Improved `random_points` by adding methods for track, hr and spatial objects.
- Adjusted `random_steps`
- Removed dependency on `velox`. This makes `extract_covariates_along` slower. 


# amt 0.0.7

## new features
- Added a dplyr count method for track, steps, random_steps and random_points
- Added a AIC method for `fit_(i)ssf`.
- Added for KDE: pi and lscv for bandwidth estimation


## major changes
- `hr_locoh_k` id deprecated and will be part of `hr_locoh`. Specifically `type = "k"`. 
- All home range methods now return `sf` objects.



# amt 0.0.6.0
## major changes
- random_points with kde home ranges now work with different home range levels. 
- random_steps now uses `units` to be explicit about the turning angle.
- `bbox` is also available for steps and can return objects of `sf`.
- `time_of_day` propgagates `NA` in coordinates.
- `time_of_day` returns factor with levels set correctly. 



# amt 0.0.5.0
## major changes
- `random_steps` gained a new distribution for random steps (exponential).
- bug fix in `random_steps`
- new function `extract_covariates_along`, extracts covariates along a random step.
- `steps` gains a new argument: `keep_cols` in order to keep columns from point when creating steps.
- `simulate_tud`: a convenience wrapper around `simulate_ud` to simulate transition UDs.
# amt 0.0.4.0
## major changes
- Added an alias to `mk_track` called `make_track`.
- `mk_track` gained a new argument `all_cols` that allows to carry over all columns to track.
- `mk_track` gained a new argument `check_duplicates` that allows to the user to specify if temporal duplicates should give an error or not.
- Added `hr_kde` to calculate Kernel home ranges.
- `steps` now uses `difftime` to calculate time differences.

## minor changes
- Removed: Depends tidyverse
- Fixed errors with lubridate::Periods within `data_frame`s

# amt 0.0.3.0
## minor changes

- Fixed vignette titles.
- Added `Rdpack` to suggest
