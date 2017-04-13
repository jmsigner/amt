#' @export
as_track <- function(x, ...) {
  UseMethod("as_track", x)
}

#' @export
as_track.SpatialPoints <- function(x, ...) {
  ## CRS should be here
  xx <- sp::coordinates(x)
  track(x = xx[, 1], y = xx[, 2])
}
