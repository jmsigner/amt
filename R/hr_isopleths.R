#' @rdname hr
#' @export
hr_isopleths <- function (x, ...) {
  UseMethod("hr_isopleths", x)
}

#' @rdname hr
#' @export
hr_isopleths.RasterLayer <- function (x, levels = 0.95, ...) {
  raster::rasterToContour(cumulative_ud(x), levels = levels, ...)
}

#' @rdname hr
#' @export
hr_isopleths.mcp <- function (x, ...) {
  x$mcp
}
