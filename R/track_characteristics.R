# to be moved
#' @useDynLib amt
#' @importFrom Rcpp sourceCpp
NULL

#' @export
velocity <- function(x, ...) {
  UseMethod("velocity", x)
}

#' @export
velocity.track_xyt <- function(x, ...) {
  print("the velocity will be calculated here....")
}

#' @export
nsd <- function(x, ...) {
  UseMethod("nsd", x)
}


#' @export
nsd.track_xy <- function(x, ...) {
  (x$x_ - x$x_[1])^2 + (x$y_ - x$y_[1])^2
}

#' @export
diff_x <- function(x, ...) {
  UseMethod("diff_x", x)
}


#' @export
diff_x.track_xy <- function(x, ...) {
   c(diff_rcpp(x$x_), NA)
}

#' @export
diff_y <- function(x, ...) {
  UseMethod("diff_y", x)
}

#' @export
diff_y.track_xy <- function(x, ...) {
   c(diff_rcpp(x$y_), NA)
}

#' Absolute Direction.
#' Function to calculate the absolute direction of a movement track. 0 is north.
#' @param x A track.
#' @name direction_abs
#'
#' @export
direction_abs <- function(x) {
  UseMethod("direction_abs", x)
}

#' @export
#' @rdname direction_abs
direction_abs.track_xy <- function(x) {
  # ((atan2(diff_y(x), diff_x(x)) / pi * 180) + 360) %% 360
  # atan2(diff_y(x), diff_x(x)) * 180 / pi
  # make north up: https://grass.osgeo.org/grass70/manuals/r.slope.aspect.html
  (450 - atan2(diff_y(x), diff_x(x)) * 180 / pi) %% 360
}

#' @export
direction_rel <- function(x, ...) {
  UseMethod("directoin_rel", x)
}

#' @export
direction_rel <- function(x) {
  c(NA, diff_rcpp(direction_abs(x)))
}


#' @export
step_lengths <- function(x, ...) {
  UseMethod("step_lengths", x)
}

#' @export
step_lengths.track_xy <- function(x, ...) {
  sqrt(step_lengths_sq(x))
}

#' @export
step_lenths_sq <- function(x, ...) {
  UseMethod("step_lengths_sq", x)
}

#' @export
step_lengths_sq <- function(x) {
  diff_x(x)^2 + diff_y(x)^2
}

#' @noRd
distance_with_diff <- function(xd, yd) {
  c(NA, sqrt((xd)^2 + (yd)^2))
}

#' @export
time_diffs <- function(x) {
  if (check_trackXYT(x)) {
    diff(as.numeric(x$t_))
  }
}

#' @export
median_time_diff <- function(x) {
  if (check_trackXYT(x)) {
    median(time_diffs(x))
  }
}

#' @export
is.regular <- function(x) {
  if (check_trackXYT(x)) {
    median(time_diffs(x))
  }
}

#' Centroid of a track
#'
#' Calcualtes the centroid of a track (i.e., the mean of the x and y coordinates).
#' @param x A track.
#' @template dots_none
#' @name centroid
#' @export
centroid <- function(x, ...) {
  UseMethod("centroid", x)
}

#' @export
#' @rdname centroid
centroid.track_xy <- function(x, ...) {
  colMeans(trk[, c("x_", "y_")])
}
