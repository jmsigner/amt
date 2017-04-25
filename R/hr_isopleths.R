#' @rdname hr
#' @export
hr_isopleths <- function (x, ...) {
  UseMethod("hr_isopleths", x)
}

#' @rdname hr
#' @export
hr_isopleths.RasterLayer <- function (x, level = 0.95, ...) {
  raster::rasterToContour(cumulative_ud(x), levels = level, ...)
}

#' @rdname hr
#' @export
hr_isopleths.mcp <- function (x, ...) {
  x$mcp
}
