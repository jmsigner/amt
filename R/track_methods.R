# Utility functions -------------------------------------------------------

#' Calculate the centroid of a track.
#'
#' @template track_xy_star
#' @param spatial `[logical(1)=FALSE]` \cr Whether or not to return a `SpatialPoints`-object.
#' @template dots_none
#' @name centroid
#' @return The centroid of a track as numeric vector if `spatial = FALSE`, otherwise as `SpatialPoints`.
#' @examples
#' data(deer)
#' centroid(deer)


#' @export
centroid <- function(x, ...) {
  UseMethod("centroid", x)
}

#' @rdname centroid
#' @export
centroid.track_xy <- function(x, spatial = FALSE, ...) {
  xx <- colMeans(x[, c("x_", "y_")])
  if (spatial) {
    sf::st_point(xx)
  } else {
    xx
  }
}

#' @export
points.track_xy <- function(x, ...) {
  graphics::points(x$x_, x$y_, ...)
}



#' Coordinates of a track.
#'
#' @template track_xy_star
#' @template dots_none
#' @return `[tibble]` \cr The coordinates.
#' @export
#' @examples
#' data(deer)
#' coords(deer)

coords <- function(x, ...) {
  UseMethod("coords", x)
}

#' @export
coords.track_xy <- function(x, ...) {
  x[, c("x_", "y_")]
}

#' @export
plot.track_xy <- function(x, ...) {
  plot(x$x_, x$y_, asp = 1, ...)
}


