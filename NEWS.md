# amt 0.0.5.0
## major changes
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
