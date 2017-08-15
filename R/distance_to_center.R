#' Distance to center.
#'
#' Calculates the distance of each pixel to the centroid of the track.
#' @param x a `track_xy`.
#' @param trast `[RasterLayer]` \cr A template.
#' @param square `[logical(1)]` \cr Should the distance be squared?
#' @param top_n `[integer(1)]` \cr To how many centers should the distance be calculated?
#' @template dots_none
#' @export
#' @return `RasterLayer`
#' @name dist_cent
distance_to_center <- function(x, ...) {
  UseMethod("distance_to_center", x)
}


#' @export
#' @rdname dist_cent
distance_to_center.track_xy <- function(x, trast, square = TRUE, ...) {
  if (missing(trast)) {
    trast <- raster::raster(as_sp(x), res = 40)
  }
  cent <- colMeans(x[, c("x_", "y_")])
  r <- raster::distanceFromPoints(trast, cent)^if(square) 2 else 1
  names(r) <- "dist_cent"
  r
}

#' @export
#' @rdname dist_cent
distance_to_centers <- function(x, ...) {
  UseMethod("distance_to_centers", x)
}

#' @export
#' @rdname dist_cent
distance_to_centers.track_xy <- function(x, trast, top_n = 10, square = TRUE, ...) {
  if (missing(trast)) {
    trast <- raster::raster(as_sp(x), res = 40)
  }
  centers <- raster::rasterize(as_sp(x), trast, fun = "count")
  pts <- raster::rasterToPoints(centers, spatial = TRUE)
  pts <- pts[order(pts$layer, decreasing = TRUE), ][1:top_n, ]
  r <- raster::distanceFromPoints(trast, pts)^if (square) 2 else 1
  names(r) <- "dist_cent"
  r
}
