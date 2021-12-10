#' Extent of a track
#'
#' Obtain the extent of a track in `x` `y` or both directions
#' @name  extent
#' @template track_xy_star_steps
#' @template dots_none
#' @return Numeric vector with the extent.

#' @export
extent_x <- function(x, ...) {
  UseMethod("extent_x", x)
}

#' @export
extent_x.track_xy <- function(x, ...) {
  xx <- diff(range_x(x))
  names(xx) <- "x_extent"
  xx
}


#' @rdname extent
#' @export
extent_y <- function(x, ...) {
  UseMethod("extent_y", x)
}

#' @export
extent_y.track_xy <- function(x, ...) {
  xx <- diff(range_y(x))
  names(xx) <- "y_extent"
  xx

}


#' @rdname extent
#' @export
extent_both <- function(x, ...) {
  UseMethod("extent_both", x)
}

#' @export
extent_both.track_xy <- function(x, ...) {
  c(extent_x(x), extent_y(x))
}


#' @export
#' @rdname extent
extent_max <- function(x, ...) {
  UseMethod("extent_max", x)
}

#' @export
extent_max.track_xy <- function(x, ...) {
  max(extent_both(x))

}
