#' Create a Track
#'
#' Consructor to crate a track, the basic building block of the `amt` pacakge. A
#' `track` is usually created from a set of `x` and `y` coordinates, possibly
#' time stamps, and any number of optional columns, such as id, sex, age, etc.
#'
#' @param .tbl A data.frame or tibble.
#' @param .x,.y,.t Literal names of columns containng the coordinates and time stamp.
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
#' @name track

mk_track <- function(tbl, .x, .y, .t, ..., crs = NULL) {

  if (missing(.x) | missing(.y)) {
    stop("x and y are required")
  }

  if (!is_tibble(tbl)) {
    tbl <- as_tibble(tbl)
  }

  vars <- quos(...)

  if (!all(sub("~", "", as.character(vars)) %in% names(tbl))) {
    stop("Non existent columns from tbl were requested.")
  }

  .x <- enquo(.x)
  .y <- enquo(.y)

  if (missing(.t)) {
    message(".t missing, creating `track_xy`.")
    out <- tbl %>%
      select(x_ = !!.x,
             y_ = !!.y,
             !!!vars)

    class(out) <- c("track_xy", class(out))

  } else {
    message(".t found, creating `track_xyt`.")
    .t <- enquo(.t)
    out <- tbl %>%
      select(x_ = !!.x,
             y_ = !!.y,
             t_ = !!.t,
             !!!vars)
    class(out) <- c("track_xyt", "track_xy", class(out))
  }

  attributes(out)$crs_ <- crs
  out




}

#' @rdname track
#' @export
track <- function(x, y, t, ..., crs = NULL) {

 # .Deprecated("mk_track", msg = "Use mk_track instead")

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

# track_transfer_attr <- function(from, to) {
#   from <- attributes(from)
#   attributes(to)$class <- from$class
#   attributes(to)$crs_ <- from$crs_
#   to
# }

 track_transfer_attr <- function(from, to) {
   from <- attributes(from)
   attributes(to)$class <- c(setdiff(from$class, class(to)), class(to))
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
arrange.track_xy <- function(.data, ..., .dots) {
  x <- NextMethod()
  track_transfer_attr(.data, x)
}

#' @export
arrange.track_xyt <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
filter.track_xy <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
filter.track_xyt <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
group_by.track_xy <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
group_by.track_xyt <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
mutate.track_xy <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
mutate.track_xyt <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}


#' @export
select.track_xy <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
select.track_xyt <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
summarise.track_xy <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
summarise.track_xyt <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
summarize.track_xy <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
summarize.track_xyt <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}


