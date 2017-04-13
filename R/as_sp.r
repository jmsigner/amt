#' @export
as_sp <- function(x, ...) {
  UseMethod("as_sp", x)
}

#' @export
as_sp.track_xy <- function(x, ...) {
  ## CRS should be here
  sp::SpatialPoints(
    coords = x[, c("x_", "y_")],
    proj4string = if (!is.null(attributes(x)$crs_)) {
      attributes(x)$crs_
    } else {
      sp::CRS(as.character(NA))
    }
  )
}
