#' Net squared displacement (nsd)
#'
#' Calculate the net squared displacement (i.e., the squared distance from the first location of a track) for a track.
#'
#' @param x A track_xy{t}.
#' @template dots_none
#' @return Numeric vector.
#' @export
nsd <- function(x, ...) {
  UseMethod("nsd", x)
}

#' @export
nsd.track_xy <- function(x, ...) {
  (x$x_ - x$x_[1])^2 + (x$y_ - x$y_[1])^2
}
