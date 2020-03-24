#' Coordinate References System (CRS)
#'
#' Check if an object has a coordinate reference system (`has_crs`) and returns the `proj4string` with `get_crs` of the coordinate reference system.
#'
#' @template any
#' @template dots_none
#' @name crs
#' @export
#' @examples
#' data(deer)
#' has_crs(deer)
#' get_crs(deer)

get_crs <- function(x, ...) {
  UseMethod("get_crs", x)
}

#' @export
get_crs.default <- function(x, ...) {
  "Not implementes for objects of this class"
}

#' @export
#' @rdname crs
has_crs <- function(x, ...) {
  UseMethod("has_crs", x)
}

#' @export
has_crs.default <- function(x, ...) {
  warning("Not implementes for objects of this class")
  FALSE
}

# track -------------------------------------------------------------------

#' @export
get_crs.track_xy <- function(x, ...) {
  attr(x, "crs", ...)
}

#' @export
has_crs.track_xy <- function(x, ...) {
  !is.na(attr(x, "crs", ...))
}


# steps -------------------------------------------------------------------

#' @export
get_crs.steps_xy <- function(x, ...) {
  attr(x, "crs", ...)
}

#' @export
has_crs.steps_xy <- function(x, ...) {
  !is.na(attr(x, "crs", ...))
}


# hr ----------------------------------------------------------------------


#' @export
get_crs.hr <- function(x, ...) {
  sf::st_crs(hr_isopleths(x))
}

#' @export
has_crs.hr <- function(x, ...) {
  !is.na(get_crs(x))
}

