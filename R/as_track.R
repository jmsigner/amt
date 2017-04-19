#' Coerces to track.
#'
#' Funcitons to coerece other classes (currently implemented: `SpatialPoints`) to a `track_xy`.
#' @export
#' @param x Object to be converted to a track.
#' @template dots_none
#' @name as_track
as_track <- function(x, ...) {
  UseMethod("as_track", x)
}

#' @export
#' @rdname as_track
as_track.SpatialPoints <- function(x, ...) {
  ## CRS should be here
  xx <- sp::coordinates(x)
  track(x = xx[, 1], y = xx[, 2], crs = sp::proj4string(x))
}
