#' Time of the day when a fix was taken
#'
#' A convenience wrapper around `suncalc::getSunlightTimes` to annotate if a fix was taken during day or night (optionally also include dawn and dusk).
#'
#' @param x `[track_xyt,steps_xyt]`\cr A track or steps.
#' @param include.crepuscule `[logical(1)=TRUE]`\cr Should dawn and dusk be included.
#' @param where `[character(1)="end"]{"start", "end", "both"}` For `steps`, should the start, end or both time points be used?
#' @template dots_none
#' @return A `tibble` with an additional column `tod_` that contains the time of the day for each relocation.
#' @name time_of_day
#' @export
#' @examples
#' data(deer)
#' deer |> time_of_day()
#' deer |> steps_by_burst() |> time_of_day()
#' deer |> steps_by_burst() |> time_of_day(where = "start")
#' deer |> steps_by_burst() |> time_of_day(where = "end")
#' deer |> steps_by_burst() |> time_of_day(where = "both")
#'


time_of_day <- function(x, ...) {
  UseMethod("time_of_day", x)
}

#' @export
#' @rdname time_of_day
time_of_day.track_xyt <- function(x, include.crepuscule = FALSE, ...) {
  validate_coords(x)
  x["tod_"] <- time_of_day_base(x, x[["t_"]], include.crepuscule = include.crepuscule, ...)
  x
}

#' @export
#' @rdname time_of_day
time_of_day.steps_xyt <- function(x, include.crepuscule = FALSE, where = "end", ...) {
  validate_coords(x)
  if (where == "both") {
    x["tod_start_"] <- time_of_day_base(x, x[["t1_"]], include.crepuscule = include.crepuscule, end = FALSE, ...)
    x["tod_end_"] <- time_of_day_base(x, x[["t2_"]], include.crepuscule = include.crepuscule, end = TRUE, ...)

  } else {
    if (where == "end") {
      x["tod_end_"] <- time_of_day_base(
        x, x[["t2_"]], include.crepuscule = include.crepuscule, end = TRUE, ...)
    } else if (where == "start") {
      x["tod_start_"] <- time_of_day_base(
        x, x[["t1_"]], include.crepuscule = include.crepuscule, end = FALSE, ...)
    }
  }
  x
}

time_of_day_base <- function(x, t, include.crepuscule, end = TRUE) {

  if (!requireNamespace("suncalc", quietly = TRUE)) {
    stop("Please install package `suncalc` first.")
  }

  if (suppressWarnings(has_crs(x))) {
    pts <- sf::st_transform(as_sf(x, end = end), 4326)
    pts <- data.frame(sf::st_coordinates(pts), as.Date(t))
    names(pts) <- c("lon", "lat", "date")
  } else {
    stop("No CRS found.")
  }

  sun <- suncalc::getSunlightTimes(data = pts, tz = lubridate::tz(pts$date))

  int.day <- lubridate::interval(sun$sunrise, sun$sunset)
  int.dawn <- lubridate::interval(sun$dawn, sun$sunrise)
  int.dusk <- lubridate::interval(sun$sunset, sun$dusk)

  tod_ <- c("night", "day")[(t %within% int.day) + 1]

  if (include.crepuscule) {
    tod_[t %within% int.dawn] <- "dawn"
    tod_[t %within% int.dusk] <- "dusk"
  }

  if (include.crepuscule) {
    factor(tod_, levels = c("day", "dusk", "night", "dawn"))
  } else {
    factor(tod_, levels = c("day", "night"))
  }
}
