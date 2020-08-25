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




# Utility functions -------------------------------------------------------

#' Calculate the centroid of a track.
#'
#' @template track_xy_star
#' @param spatial `[logical(1)=FALSE]` \cr Whether or not to return a `SpatialPoints`-object.
#' @template dots_none
#' @name centroid
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
    sp::SpatialPoints(cbind(xx["x_"], xx["y_"]))
  } else {
    xx
  }
}



#' @export
points.track_xy <- function(x, ...) {
  graphics::points(x[, c("x_", "y_")], ...)
}



#' Coordinates of a track.
#'
#' @template track_xy_star
#' @template dots_none
#' @return `[tibble]` \cr The coordinates.
#' @export
#' @name helper
#' @examples
#' data(deer)
#' coords(deer)

coords <- function(x, ...) {
  UseMethod("coords", x)
}

#' @export
#' @rdname  helper
coords.track_xy <- function(x, ...) {
  x[, c("x_", "y_")]
}

#' @export
plot.track_xy <- function(x, ...) {
  plot(x$x_, x$y_, ...)
}

#' @rdname  helper
#' @export
make_trast <- function(x, ...) {
  UseMethod("make_trast", x)
}

#' @export
#' @rdname  helper
#' @param factor `[numeric(1)=1.5]{>= 1}`\cr Factor by which the extent of the relocations is extended.
#' @param res `[numeric(1)]`\cr Resolution of the output raster.
make_trast.track_xy <- function(x, factor = 1.5, res = max(c(extent_max(x) / 100, 1e-9)), ...) {

  checkmate::assert_number(factor, lower = 1)
  checkmate::assert_number(res, lower = 1e-10)

  me <- extent_max(x)
  bu <- me * factor - me
  raster::raster(amt::bbox(x, buffer = bu), res = res)


}

#' @rdname  helper
#' @export
extent_x <- function(x, ...) {
  UseMethod("extent_x", x)
}

#' @rdname  helper
#' @export
extent_x.track_xy <- function(x, ...) {
  xx <- diff(range_x(x))
  names(xx) <- "x_extent"
  xx
}


#' @rdname  helper
#' @export
extent_y <- function(x, ...) {
  UseMethod("extent_y", x)
}

#' @rdname  helper
#' @export
extent_y.track_xy <- function(x, ...) {
  xx <- diff(range_y(x))
  names(xx) <- "y_extent"
  xx

}


#' @rdname  helper
#' @export
extent_both <- function(x, ...) {
  UseMethod("extent_both", x)
}

#' @rdname  helper
#' @export
extent_both.track_xy <- function(x, ...) {
  c(extent_x(x), extent_y(x))
}


#' @export
#' @rdname  helper
extent_max <- function(x, ...) {
  UseMethod("extent_max", x)
}

#' @rdname  helper
#' @export
extent_max.track_xy <- function(x, ...) {
  max(extent_both(x))

}

# Range methods -----------------------------------------------------------
#' @rdname  helper
#' @export
range_x <- function(x, ...) {
  UseMethod("range_x", x)
}


#' @rdname  helper
#' @export
range_x.track_xy <- function(x, ...) {
  xx <- range(x$x_)
  names(xx) <- c("x_min", "x_max")
  xx
}


#' @rdname  helper
#' @export
range_y <- function(x, ...) {
  UseMethod("range_y", x)
}

#' @rdname  helper
#' @export
range_y.track_xy <- function(x, ...) {
  xx <- range(x$y_)
  names(xx) <- c("y_min", "y_max")
  xx
}

#' @rdname  helper
#' @export
range_both <- function(x, ...) {
  UseMethod("range_both", x)
}

#' @rdname  helper
#' @export
range_both.track_xy <- function(x, ...) {
  c(range_x(x), range_y(x))
}

