#' Working with home ranges
#'
#' Function to calculate the area and isopleths of a home range.
#'
#' @param x Fitted home range.
#' @param level Numeric scalar, home-range level.
#' @template dots_none
#' @name hr
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
