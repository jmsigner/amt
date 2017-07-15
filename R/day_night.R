#' Extract if a fix was taken during day or night.
#'
#' A convinience wrapper around `maptools::sunriset`.
#'
#' @param x A `track_xyt` or `steps_xyt`
#' @template dots_none
#' @name day_night
#' @export
day_night <- function(x, ...) {
  UseMethod("day_night", x)
}

#' @export
day_night.track_xyt <- function(x, ...) {
  # has crs?
  day <- lubridate::interval(
    maptools::sunriset(as_sp(x), x$t_, direction = "sunrise", POSIXct.out = TRUE)$time,
    maptools::sunriset(as_sp(x), x$t_, direction = "sunset", POSIXct.out = TRUE)$time
  )
  lubridate::`%within%`(x$t_, day)
}

#' @param end A logical scalar, indicating if the time stemp of the end of the step should be used.
#' @rdname day_night
#' @export
day_night.steps <- function(x, end = TRUE, ...) {
  # has crs?
  x <- stps
  if (has_crs(x)) {
    pts <- sp::spTransform(as_sp(x, end = TRUE), sp::CRS("+init=epsg:4326"))
  } else {
    stop("No CRS found.")
  }

  if (!end) {
    stop("not yet implemented")
  }

  day <- lubridate::interval(
    maptools::sunriset(pts, x$t2_, direction = "sunrise", POSIXct.out = TRUE)$time,
    maptools::sunriset(pts, x$t2_, direction = "sunset", POSIXct.out = TRUE)$time
  )
  ifelse(lubridate::`%within%`(x$t2_, day), "day", "night")
}
