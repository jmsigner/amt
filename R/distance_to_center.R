#' Distance to center
#'
#' Distances to frequently used areas. `distance_to_center` calculates the
#' distance to the home-range center (i.e., the centroid of the `x` and `y`
#' coordinates). `distance_to_centers` calculates the distance to `top_n` most
#' frequently used cells. Note, that the results of `distance_to_center` is
#' different to `distance_to_centers` with `top_n = 1`, since in the first case
#' the distance to the centroid is calculated and in the second case the
#' distance to the raster cell with the most relocations.
#' @template track_xy_star
#' @param trast `[RasterLayer]` \cr A template.
#' @param square `[logical(1)]` \cr Should the distance be squared?
#' @param top_n `[integer(1)]` \cr To how many centers should the distance be
#'   calculated?
#' @template dots_none
#' @export
#' @return `RasterLayer`
#' @name dist_cent
#' @examples
#' data(deer)
#' r <- raster::raster(bbox(deer, buffer = 100), res = 40)
#' d1 <- distance_to_center(deer, r)
#' d2 <- distance_to_centers(deer, r, top_n = 1)
#' d3 <- distance_to_centers(deer, r, top_n = 10)
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
distance_to_center.numeric <- function(x, trast, square = TRUE, ...) {
  if (missing(trast)) {
    stop("trast required")
  }
  r <- raster::distanceFromPoints(trast, x)^if(square) 2 else 1
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
