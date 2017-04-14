#' Create a Track
#'
#' Consructor to crate a track, the basic building block of the `amt` pacakge. A
#' `track` is usually created from a set of `x` and `y` coordinates, possibly
#' time stamps, and any number of optional columns, such as id, sex, age, etc.
#'
#' @param x A *required* numeric vector, containing the x-coordinates (=
#'   latitude).
#' @param y A *required* numeric vector, containing the y-coordinates (=
#'   longitude).
#' @param t An *optional* vector of  `POSIXct` time stamps.
#' @param ... Additional columns, that should be given in the form of `key =
#'   val` (e.g., for ids this may look like this `id = c(1, 1, 1, 2, 2, 2` for
#'   three points for ids 1 and 2 each).
#' @param crs An optional coordinate reference system of the points.
#' @return If `t` was provided an object of class `track_xyt` is returned
#'   otherwise a `track_xy`.
#' @export
track <- function(x, y, t, ..., crs = NULL) {

  if (missing(x) | missing(y)) {
    stop("x and y are required")
  }

  if (missing(t)) {
    out <- tibble(
      x_ = x,
      y_ = y,
      ...
    )
   class(out) <- c("track_xy", class(out))

  } else {
    out <- tibble(
      x_ = x,
      y_ = y,
      t_ = t,
      ...
    )
   class(out) <- c("track_xyt", "track_xy", class(out))
  }

  attributes(out)$crs_ <- crs
  out
}


# S3 Methods --------------------------------------------------------------

 track_transfer_attr <- function(from, to) {
   from <- attributes(from)
   attributes(to)$class <- from$class
   attributes(to)$crs_ <- from$crs_
   to
 }

# tibble methods
#' @export
`[.track_xy` <- function(x, i, j, drop = FALSE) {
  xx <- NextMethod()
  track_transfer_attr(x, xx)
}


#' @export
`[.track_xyt` <- function(x, i, j, drop = FALSE) {
  xx <- NextMethod()
  track_transfer_attr(x, xx)
}

# dplyr methods -----------------------------------------------------------


#' @export
arrange_.track_xy <- function(.data, ..., .dots) {
  x <- NextMethod()
  track_transfer_attr(.data, x)
}

#' @export
arrange_.track_xyt <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
filter_.track_xy <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
filter_.track_xyt <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
group_by_.track_xy <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
group_by_.track_xyt <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
mutate_.track_xy <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
mutate_.track_xyt <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}


#' @export
select_.track_xy <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
select_.track_xyt <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
summarise_.track_xy <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
summarise_.track_xyt <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
summarzse_.track_xy <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
summarzse_.track_xyt <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}


