# amt 0.0.9
## bug fixes
- `extract_covariate` now works with new version of tibble for raster stacks.
- `fit_distr` for gamma distribution, now replaces 0 step length with min setep- length and informs the user with a message. 

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
- Improved `random_points` by addeing methods for track, hr and spatial objects.
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
- `simulate_tud`: a convenience wrapper arround `simulate_ud` to simulate transition UDs.
# amt 0.0.4.0
## major changes
- Added an alias to `mk_track` called `make_track`.
- `mk_track` gained a new argument `all_cols` that allows to carry over all columns to track.
- `mk_track` gained a new argument `check_duplicates` that allows to the user to specify if temporal duplicates should give an error or not.
- Added `hr_kde` to calculate Kernel home ranges.
- `steps` now uses `difftime` to calcualte time differences.

## minor changes
- Removed: Depends tidyverse
- Fixed errors with lubridate::Periods within `data_frame`s

# amt 0.0.3.0
## minor changes

- Fixed vignette titels.
- Added `Rdpack` to suggest
