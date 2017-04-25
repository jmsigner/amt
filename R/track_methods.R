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

#' @export
#' @rdname track_methods
direction_abs <- function(x) {
  UseMethod("direction_abs", x)
}

#' @export
#' @rdname track_methods
direction_abs.track_xy <- function(x) {
  # ((atan2(diff_y(x), diff_x(x)) / pi * 180) + 360) %% 360
  # atan2(diff_y(x), diff_x(x)) * 180 / pi
  # make north up: https://grass.osgeo.org/grass70/manuals/r.slope.aspect.html
  (450 - atan2(diff_y(x), diff_x(x)) * 180 / pi) %% 360
}

#' @export
#' @rdname track_methods
direction_rel <- function(x, ...) {
  UseMethod("directoin_rel", x)
}

#' @export
#' @rdname track_methods
direction_rel <- function(x) {
  c(NA, diff_rcpp(direction_abs(x)))
}


#' @export
#' @rdname track_methods
step_lengths <- function(x, ...) {
  UseMethod("step_lengths", x)
}

#' @export
#' @rdname track_methods
step_lengths.track_xy <- function(x, ...) {
  sqrt(step_lengths_sq(x))
}

#' @export
#' @rdname track_methods
step_lenths_sq <- function(x, ...) {
  UseMethod("step_lengths_sq", x)
}

#' @export
#' @rdname track_methods
step_lengths_sq <- function(x) {
  diff_x(x)^2 + diff_y(x)^2
}

#' @noRd
distance_with_diff <- function(xd, yd) {
  c(NA, sqrt((xd)^2 + (yd)^2))
}



#' @export
#' @rdname track_methods
centroid <- function(x, ...) {
  UseMethod("centroid", x)
}

#' @export
#' @rdname track_methods
centroid.track_xy <- function(x, ...) {
  colMeans(x[, c("x_", "y_")])
}
