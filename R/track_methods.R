#' Track Methods
#'
#' Methods to work with a track.
#' Function to calculate the absolute direction of a movement track. 0 is north.
#'
#' @param x A track_xy{t}.
#' @template dots_none
#' @name track_methods
#' @export
velocity <- function(x, ...) {
  UseMethod("velocity", x)
}

#' @export
#' @rdname track_methods
velocity.track_xyt <- function(x, ...) {
  print("the velocity will be calculated here....")
}

#' @export
#' @rdname track_methods
nsd <- function(x, ...) {
  UseMethod("nsd", x)
}


#' @export
#' @rdname track_methods
nsd.track_xy <- function(x, ...) {
  (x$x_ - x$x_[1])^2 + (x$y_ - x$y_[1])^2
}

#' @export
#' @rdname track_methods
diff_x <- function(x, ...) {
  UseMethod("diff_x", x)
}


#' @export
#' @rdname track_methods
diff_x.track_xy <- function(x, ...) {
   c(diff_rcpp(x$x_), NA)
}

#' @export
#' @rdname track_methods
diff_y <- function(x, ...) {
  UseMethod("diff_y", x)
}

#' @export
#' @rdname track_methods
diff_y.track_xy <- function(x, ...) {
   c(diff_rcpp(x$y_), NA)
}


# Directions --------------------------------------------------------------
#' @rdname direction
#' @export
#' @examples
direction_rel <- function(x, ...) {
  UseMethod("directoin_rel", x)
}

#' @export
#' @rdname direction
direction_rel <- function(x, degrees = TRUE) {
  p <- c(NA, diff_rcpp(direction_abs(x, degrees = FALSE)))
  p <- ifelse( p <= (-pi), p + 2 * pi, p)
  p <- ifelse( p > pi, p - 2 * pi, p)
  p * if (degrees) 180 / pi else 1
}



# step lengths ------------------------------------------------------------

#' Step lengths
#'
#' Funcitons to calculate step lenghts
#'
#' @export
#' @param x A track.
#' @param lonlat Logcial scalar, if `TRUE` geogrphic distances are calculated
#' @name step_length

#' @export
#' @rdname step_length
#' @examples
#' step_lengths(trk)
#' step_lengths(trk, lonlat = TRUE)
#'
#' m <- move::move(x, y, now() + hours(1:10), proj = CRS("+init=epsg:4326"))
#' move::distance(m)
#'
step_lengths <- function(x, ...) {
  UseMethod("step_lengths", x)
}

#' @export
#' @rdname step_length
step_lengths.track_xy <- function(x, lonlat = FALSE, ...) {
  if (lonlat) {
    pts <- sp::coordinates(as_sp(x))
    raster::pointDistance(pts[-nrow(pts), ], pts[-1, ], lonlat = TRUE)
  } else {
    sqrt(step_lengths_sq(x))
  }
}

#' @export
#' @rdname step_length
step_lengths_sq <- function(x, ...) {
  UseMethod("step_lengths_sq", x)
}

#' @export
#' @rdname step_length
step_lengths_sq.track_xy <- function(x) {
  diff_x(x)^2 + diff_y(x)^2
}

#' @noRd
distance_with_diff <- function(xd, yd) {
  c(NA, sqrt((xd)^2 + (yd)^2))
}



# Utility functions -------------------------------------------------------

#' Centroid of a track
#'
#' Calcualte the centroid of a track.
#'
#' @param x A track.
#' @template dots_none
#' @example
#' data(deer)
#' centroid(deer)


#' @export
centroid <- function(x, ...) {
  UseMethod("centroid", x)
}

#' @export
centroid.track_xy <- function(x, ...) {
  colMeans(x[, c("x_", "y_")])
}


#' Coordinate References System
#'
#' Get the Coordinate Reference System (CRS) of a track.
#'
#' @param x A track.
#' @template dots_none
#' @export
#' @example
#' data(deer)
#' get_crs(deer)

get_crs <- function(x, ...) {
  UseMethod("get_crs", x)
}

#' @export
get_crs.track_xy <- function(x, ...) {
  attr(x, "crs", ...)
}


#' Get bounding box of a track
#' @param x A track.
#' @param spatial Logical, whether or not to return a `sp`-object.
#' @param buffer Numeric, an optional buffer.
#' @export
#' @example
#' data(deer)
#' bbox(deer)
#' bbox(deer, buffer = 100)

bbox <- function(x, ...) {
  UseMethod("bbox", x)
}

#' @export
bbox.track_xy <- function(x, spatial = TRUE, buffer = NULL) {
  bbx <- rgeos::gEnvelope(as_sp(x))
  if (!is.null(buffer)) {
    bbx <- rgeos::gEnvelope(rgeos::gBuffer(bbx, width = buffer))
  }
  if (spatial) {
    bbx
  } else {
    sp::bbox(bbx)
  }
}
