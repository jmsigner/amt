#' Distance to center.
#'
#' Calculates the distance of each pixel to the centroid of the track.
#' @param x a `track_xy`.
#' @template dots_none
#' @export
#' @return `RasterLayer`
#' @name distance_to_center
distance_to_center <- function(x, ...) {
  UseMethod("distance_to_center", x)
}


#' @export
#' @param trast A template `RasterLayer`.
#' @rdname distance_to_center
distance_to_center.track_xy <- function(x, trast, ...) {
  if (missing(trast)) {
    trast <- raster::raster(as_sp(x), res = 40)
  }
  cent <- colMeans(x[, c("x_", "y_")])
  r <- raster::distanceFromPoints(trast, cent)
  names(r) <- "dist_cent"
  r
}
