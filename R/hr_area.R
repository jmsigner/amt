#' @export
hr_area <- function(x, ...) {
  UseMethod("hr_area", x)
}

#' @export
hr_area.mcp <- function(x, ...) {
  as_data_frame(x$mcp)
}

#' @export
hr_area.locoh <- function(x, ...) {
  as_data_frame(x)
}

#' @export
hr_area.RasterLayer <- function(x, level = 0.95, ...) {
    x <- cumulative_ud(x)
    sum(x[] <= level) * prod(raster::res(x))
}
