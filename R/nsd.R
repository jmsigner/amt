#' Net squared displacement (nsd)
#'
#' The function `nsd()` calculates the net squared displacement (i.e., the squared distance from the first location of a track) for a track. The function `add_nsd()` add a new column to a track or steps object with the nsd (the column name is `nsd_`).
#'
#' @template track_xy_star
#' @template dots_none
#' @return Numeric vector (for `nsd()`) and a tillbe with the original data with a new column for `add_nsd()`.
#' @name nsd
#' @export
nsd <- function(x, ...) {
  UseMethod("nsd", x)
}

#' @export
#' @rdname nsd
nsd.track_xy <- function(x, ...) {
  (x$x_ - x$x_[1])^2 + (x$y_ - x$y_[1])^2
}

#' @export
#' @rdname nsd
add_nsd <- function(x, ...) {
  UseMethod("add_nsd", x)
}

#' @export
#' @rdname nsd
add_nsd.track_xy <- function(x, ...) {
  x[["nsd_"]] <- nsd(x)
  x
}

#' @export
#' @rdname nsd
add_nsd.steps_xy <- function(x, ...) {
  x[["nsd_"]] <- (x$x2_ - x$x1_[1])^2 + (x$y2_ - x$y1_[1])^2
  x
}
