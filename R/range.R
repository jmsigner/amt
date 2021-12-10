#' Geographic range
#'
#' The range that in either `x`, `y` or `both` directions, that a track covers.
#' @template track_xy_star
#' @template dots_none
#' @return Numeric vector with the range.

#' @name range
#' @export
range_x <- function(x, ...) {
  UseMethod("range_x", x)
}


#' @export
#' @rdname range
range_x.track_xy <- function(x, ...) {
  xx <- range(x$x_)
  names(xx) <- c("x_min", "x_max")
  xx
}


#' @rdname range
#' @export
range_y <- function(x, ...) {
  UseMethod("range_y", x)
}

#' @export
#' @rdname range
range_y.track_xy <- function(x, ...) {
  xx <- range(x$y_)
  names(xx) <- c("y_min", "y_max")
  xx
}

#' @rdname range
#' @export
range_both <- function(x, ...) {
  UseMethod("range_both", x)
}

#' @export
#' @rdname range
range_both.track_xy <- function(x, ...) {
  c(range_x(x), range_y(x))
}
