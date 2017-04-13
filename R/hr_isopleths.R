#' @export
hr_isopleths <- function (x, ...) {
  UseMethod("hr_isopleths", x)
}

#' @export
hr_isopleths.RasterLayer <- function (x, levels = 0.95, ...) {
  raster::rasterToContour(cumulative_ud(x), levels = levels, ...)
}

#' @export
hr_isopleths.mcp <- function (x, ...) {
  x$mcp
}
