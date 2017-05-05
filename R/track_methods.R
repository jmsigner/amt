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
#' Directions
#'
#' Functions to calculate the relative and absolute direction of an animal
#'
#' Anti clockwise angles of steps to an absolute direction (North, East, South or West).
#' @export
#' @param x A track.
#' @param degrees Logcial scalar, if `TRUE` angles are returned in degrees, otherwise in radians.
#' @param full_circle Logcial scalar, if `TRUE` angles are returned between 0 and 360 degrees or 0 and 2 pi (depnding on the value of `degrees`), otherwise angles are between -180 and 180 or -pi and pi.
#' @param zero_dir Character scalar, must be either 'N', 'E', 'S' or 'W' and indicates the zero direction.
#' @param clockwise Logical scalar, should angles be calculated clock or anti-clockwise.
#' @param append_last Logical scalar, if `TRUE` an `NA` is appended at the end of all angles.
#' @name direction
direction_abs <- function(x, ...) {
  UseMethod("direction_abs", x)
}

#' @export
#' @rdname direction
#' @examples
#' # Absolute directions
#'


direction_abs.track_xy <- function(x, degrees = TRUE, full_circle = FALSE, zero_dir = "E", clockwise = FALSE,
                                   append_last = TRUE, planar = TRUE, ...) {
  zero_dir <- toupper(zero_dir)
  if (!zero_dir %in% c("E", "N", "W", "S")) {
    stop("zero_dir should be in either 'E', 'N', 'W', or 'S'")
  }

  # angles

  a <- if (planar) {
    atan2(diff_y(x), diff_x(x)) * 180 / pi
  } else {
    xx <- sp::coordinates(as_sp(x))
    (450 + ((360 - geosphere::bearing(xx[-nrow(xx), ], xx[-1, ]))) %% 360) %% 360
  }


  # remove last NA
  a <- if (!append_last) a[-length(a)] else a

  a <- ifelse(a < 0, 360 + a, a)

  a <- switch(zero_dir,
         E = a,
         S = (450 + a) %% 360,
         W = (540 + a) %% 360,
         N = (630 + a) %% 360
  )

  a <- if (clockwise)  (360 - a) %% 360 else a
  a <- if (full_circle) a else ifelse(a > 180, (360 - a) * -1, a)
  a * if (degrees) 1 else pi / 180
}

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
