#' Extract covariate values
#'
#' Extract the covariate values at relocations, or at the beginning or end of
#' steps.
#' @template track_xy_star_steps
#' @param covariates `[RasterLayer,RasterStack,RasterBrick]` \cr The
#'   (environmental) covariates. For `extract_covariates_var_time` the argument
#'   `covariates` need to have a `z`-column (i.e. the time stamp).
#' @param where `[character(1)="end"]{"start", "end", "both"}` \cr For `steps` this
#'   determines if the covariate values should be extracted at the beginning or
#'   the end of a step. or `end`.
#' @template dots_none
#' @name extract_covariates
#' @export
#' @examples
#' data(deer)
#' data(sh_forest)
#' deer %>% extract_covariates(sh_forest)
#' deer %>% steps %>% extract_covariates(sh_forest)
#' deer %>% steps %>% extract_covariates(sh_forest, where = "start")

extract_covariates <- function(x, ...) {
  UseMethod("extract_covariates", x)
}

#' @export
#' @rdname extract_covariates
extract_covariates.track_xy <- function(x, covariates, ...) {
  extract_covar_base(x, covariates)
}

#' @export
#' @rdname extract_covariates
extract_covariates.random_points <- function(x, covariates, ...) {
  extract_covar_base(x, covariates)
}

#' @export
#' @rdname extract_covariates
extract_covariates.steps_xy <- function(x, covariates, where = "end", ...) {
  if (class(covariates) %in% paste0("Raster", c("Layer", "Stack", "Brick"))) {
    if (where == "both") {
      x_start <- raster::extract(covariates, as.matrix(x[, c("x1_", "y1_")]),
                                 df = TRUE)[, -1, drop = FALSE]
      names(x_start) <- paste0(names(x_start), "_start")
      x_end <- raster::extract(covariates, as.matrix(x[, c("x2_", "y2_")]),
                               df = TRUE)[, -1, drop = FALSE]
      names(x_end) <- paste0(names(x_end), "_end")
      x_all <- cbind(x_start, x_end)
      x[names(x_all)] <- as.data.frame(x_all)
    } else {
      x[names(covariates)] <- if (where == "end") {
        as.data.frame(raster::extract(covariates, as.matrix(x[, c("x2_", "y2_")])))
      } else if (where == "start") {
        as.data.frame(raster::extract(covariates, as.matrix(x[, c("x1_", "y1_")])))
      }
    }
    x
  } else {
    stop("no raster")
  }
}

extract_covar_base <- function(x, covars) {
  if (class(covars) %in% paste0("Raster", c("Layer", "Stack", "Brick"))) {
    x[names(covars)] <- as.data.frame(raster::extract(covars, x[, c("x_", "y_")]))
    x
  } else {
    stop("no raster")
  }
}


# extract covariates along ------------------------------------------------


#' @rdname extract_covariates
#' @details `extract_covariates_along` extracts the covariates along a straight line between the start and the end point of a (random) step. It returns a list, which in most cases will have to be processed further.
#' @export
#' @examples
#' \dontrun{
#' data(deer) # relocation
#' data("sh_forest") # env covar
#'
#' p1 <- deer %>% steps() %>% random_steps() %>%
#'   extract_covariates(sh_forest) %>% # extract at the endpoint
#'   mutate(for_path = extract_covariates_along(., sh_forest))  %>%
#'   # 1 = forest, lets calc the fraction of forest along the path
#'   mutate(for_per = purrr::map_dbl(for_path, ~ mean(. == 1)))
#' }
#'
extract_covariates_along <- function(x, ...) {
  UseMethod("extract_covariates_along", x)
}

#' @export
#' @rdname extract_covariates
extract_covariates_along.steps_xy <- function(x, covariates, ...) {
  if (class(covariates) %in% paste0("Raster", c("Layer", "Stack", "Brick"))) {
    wkt <- with(x, paste0("LINESTRING (", x1_, " ", y1_, ",", x2_, " ", y2_, ")"))
    ll <- sf::st_as_sfc(wkt)
    # Remove dependency on velox
    # v <- velox::velox(covariates)
    # l2 <- v$extract(sp = ll)
    l2 <- raster::extract(covariates, sf::as_Spatial(ll))
    return(l2)
  } else {
    stop("covariates: need to be a Raster*.")
  }
}


# Extract covariates varying time -----------------------------------------
#' @rdname extract_covariates
#' @param when `[character(1)="any"]{"any", "before", "after"}` \cr Specifies for
#'  for `extract_covariates_var_time` whether to look before, after or in both
#'  direction (`any`) for the temporally closest environmental raster.
#' @param max_time `[Period(1)]` \cr The maximum time difference between a relocation
#'  and the corresponding raster. If no rasters are within the specified max.
#'  distance `NA` is returned.
#' @param name_covar `[character(1)="time_var_covar"]` \cr The name of the new column.
#' @export
#' @examples
#' # Simulate some dummy data
#' # Hourly data for 10 days: 24 * 10
#' set.seed(123)
#' path <- data.frame(x = cumsum(rnorm(240)),
#'               y = cumsum(rnorm(240)),
#'               t = lubridate::ymd("2018-01-01") + hours(0:239))
#' trk <- make_track(path, x, y, t)
#'
#' # dummy env data
#' rs <- raster::raster(xmn = -50, xmx = 50, ymn = -50, ymx = 50, res = 1)
#'
#' # create dummy covars for each day
#' rs <- raster::stack(lapply(1:10, function(i)
#'   raster::setValues(rs, runif(1e4, i - 1, i))))
#'
#' # Env covariates are always taken at noon
#' rs <- raster::setZ(rs, lubridate::ymd_hm("2018-01-01 12:00") + days(0:9))
#'
#' # Allow up to 2 hours after
#' trk %>% extract_covariates_var_time(rs, max_time = hours(2), when = "after") %>%
#'   print(n = 25)
#' trk %>% extract_covariates_var_time(rs, max_time = hours(2), when = "before") %>%
#'   print(n = 25)
#' trk %>% extract_covariates_var_time(rs, max_time = hours(2), when = "any") %>%
#'   print(n = 25)
#'
#' # We can use different time scales
#' trk %>%
#'   extract_covariates_var_time(
#'     rs, max_time = hours(2), when = "any", name_covar = "env_2h") %>%
#'   extract_covariates_var_time(
#'     rs, max_time = hours(4), when = "any", name_covar = "env_4h") %>%
#'   extract_covariates_var_time(
#'     rs, max_time = hours(6), when = "any", name_covar = "env_6h") %>%
#'   print(n = 25)
#'
#' # We can use different time scales: after
#' trk %>%
#'   extract_covariates_var_time(
#'     rs, max_time = hours(2), when = "after", name_covar = "env_2h") %>%
#'   extract_covariates_var_time(
#'     rs, max_time = hours(4), when = "after", name_covar = "env_4h") %>%
#'   extract_covariates_var_time(
#'     rs, max_time = hours(6), when = "after", name_covar = "env_6h") %>%
#'   print(n = 25)
#'
#' # We can use different time scales: before
#' trk %>%
#'   extract_covariates_var_time(
#'     rs, max_time = hours(2), when = "before", name_covar = "env_2h") %>%
#'   extract_covariates_var_time(
#'     rs, max_time = hours(4), when = "before", name_covar = "env_4h") %>%
#'   extract_covariates_var_time(
#'     rs, max_time = hours(6), when = "before", name_covar = "env_6h") %>%
#'   print(n = 25)
#'
#' # The same works also for steps
#' trk %>%
#'   steps() %>%
#'   extract_covariates_var_time(
#'     rs, max_time = hours(2), when = "before", name_covar = "env_2h") %>%
#'   print(n = 25)
#'
#' # also with start and end
#' trk %>%
#'   steps() %>%
#'   extract_covariates_var_time(
#'     rs, max_time = hours(2), when = "before", name_covar = "env_2h",
#'     where = "both") %>%
#'   print(n = 25)
#'
extract_covariates_var_time <- function(x, ...) {
  UseMethod("extract_covariates_var_time", x)
}

#' @export
#' @rdname extract_covariates
extract_covariates_var_time.track_xyt <- function(
  x, covariates, when = "any", max_time,
  name_covar = "time_var_covar", ...) {
  x[name_covar] <- extract_covar_var_time_base(
    cbind(x$x_, x$y_),
    x$t_, covariates, when, max_time)
  x
}

#' @export
#' @rdname extract_covariates
extract_covariates_var_time.steps_xyt <- function(
  x, covariates, when = "any", max_time, name_covar = "time_var_covar",
  where = "end", ...) {

  if (where == "start") {
    x[name_covar] <- extract_covar_var_time_base(
      cbind(x$x1_, x$y1_),
      x$t1_, covariates, when, max_time)
  } else if (where == "end") {
    x[name_covar] <- extract_covar_var_time_base(
      cbind(x$x2_, x$y2_),
      x$t2_, covariates, when, max_time)
  } else if (where == "both") {
    x[paste0(name_covar, "_start")] <- extract_covar_var_time_base(
      cbind(x$x1_, x$y1_),
      x$t1_, covariates, when, max_time)
    x[paste0(name_covar, "_end")] <- extract_covar_var_time_base(
      cbind(x$x2_, x$y2_),
      x$t2_, covariates, when, max_time)
  }
  x
}


extract_covar_var_time_base <- function(
  xy, t, covariates, when = "any",
  max_diff) {

  if (is.null(raster::getZ(covariates))) {
    stop("Covariates do not have a Z column.")
  }

  if (!is(max_diff, "Period")) {
    stop("`max_diff` is not of class `Period`.")
  }
  max_diff <- lubridate::period_to_seconds(max_diff)
  t_covar <- as.numeric(as.POSIXct(raster::getZ(covariates)))
  t_obs <- as.numeric(as.POSIXct(t))

  # Fun to find closest point
  which_rast <- function(t_diffs, where, max_diff) {
    wr <- if (when == "after") {
      which.min(t_diffs[t_diffs >= 0])
    } else if (when == "before") {
      which.min(abs(t_diffs[t_diffs <= 0])) + sum(t_diffs > 0)
    } else if (when == "any") {
      which.min(abs(t_diffs))
    }
    if (length(wr) == 0) {
      NA
    } else if (max_diff < abs(t_diffs[wr])) {
      NA
    } else {
      wr
    }
  }

  wr <- sapply(t_obs, function(x) which_rast(x - t_covar, when, max_diff))
  ev <- raster::extract(covariates, cbind(xy))
  cov_val <- ev[cbind(seq_along(wr), wr)]
  return(cov_val)
}



