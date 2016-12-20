# to be moved
#' @useDynLib amt
#' @importFrom Rcpp sourceCpp
NULL

#' @export
angle_abs <- function(x) {
  is_trackXY(x)
}

#' @export
angle_rel <- function(x) {
  is_trackXY(x)
}

#' @export
velocity <- function(x) {
  is_trackXY(x)
}

#' @export
nsd <- function(x) {
  if (check_trackXY(x)) {
    (x$x_ - x$x_[1])^2 + (x$y_ - x$y_[1])^2
  }
}

#' @export
diff_x <- function(x) {
  if (check_trackXY(x)) {
   c(NA, diff_rcpp(x$x_))
  }
}

#' @export
diff_y <- function(x) {
  if (check_trackXY(x)) {
   c(NA, diff_rcpp(x$y_))
  }
}


#' @export
direction_abs <- function(x) {
  if (check_trackXY(x)) {
    c(NA, ((atan2(diff_y(x), diff_x(x)) / pi * 180) + 360) %% 360)
  }
}

direction_rel <- function(x) {
  if (check_trackXY(x)) {
    c(NA, diff_rcpp(direction_abs(x)))
  }
}


#' @export
distance <- function(x) {
  if (check_trackXY(x)) {
    c(sqrt((diff_x(x))^2 + (diff_y(x))^2))
  }
}

#' @noRd
distance_with_diff <- function(xd, yd) {
  c(NA, sqrt((xd)^2 + (yd)^2))
}

