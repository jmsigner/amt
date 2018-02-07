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
    sp::SpatialPoints(cbind(xx$x_, xx$y_))
  } else {
    xx
  }
}



#' Get bounding box of a track.
#' @template track_xy_star
#' @param spatial `[logical(1)=FALSE]` \cr Whether or not to return a `SpatialPolygons`-object or not.
#' @param buffer `[numeric(0)=NULL]{NULL, >0}` \cr An optional buffer of the bounding box.
#' @template dots_none
#' @name bbox
#' @export
#' @examples
#' data(deer)
#' bbox(deer)
#' bbox(deer, buffer = 100)

bbox <- function(x, ...) {
  UseMethod("bbox", x)
}

#' @export
#' @rdname bbox
bbox.track_xy <- function(x, spatial = TRUE, buffer = NULL, ...) {
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


#' @export
points.track_xy <- function(x, ...) {
  graphics::points(x[, c("x_", "y_")], ...)
}



#' Coordinates of a track.
#'
#' @template track_xy_star
#' @template dots_none
#' @return `[data_frame]` \cr The coordinates.
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
  plot(x$x_, x$y_, ...)
}
