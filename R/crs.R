#' Coordinate References System
#'
#' Get the Coordinate Reference System (CRS) of a track.
#'
#' @param x A track.
#' @template dots_none
#' @name crs
#' @export
#' @examples
#' data(deer)
#' get_crs(deer)

get_crs <- function(x, ...) {
  UseMethod("get_crs", x)
}

#' @export
#' @rdname crs
has_crs <- function(x, ...) {
  UseMethod("has_crs", x)
}


# track -------------------------------------------------------------------

#' @export
get_crs.track_xy <- function(x, ...) {
  attr(x, "crs", ...)
}

#' @export
has_crs.track_xy <- function(x, ...) {
  !is.null(attr(x, "crs", ...))
}


# steps -------------------------------------------------------------------

#' @export
get_crs.steps <- function(x, ...) {
  attr(x, "crs", ...)
}

#' @export
has_crs.steps <- function(x, ...) {
  !is.null(attr(x, "crs", ...))
}


# hr ----------------------------------------------------------------------


#' @export
get_crs.hr <- function(x, ...) {
  attr(x, "crs", ...)
  sp::proj4string(hr_isopleths(x))
}

#' @export
has_crs.hr <- function(x, ...) {
  !is.na(get_crs(x))
}

