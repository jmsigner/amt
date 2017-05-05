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
#' @rdname hr
hr_area.mcp <- function(x, ...) {
  as_data_frame(x$mcp)
}

#' @export
#' @rdname hr
hr_area.locoh <- function(x, ...) {
  as_data_frame(x)
}

#' @export
#' @param level Numeric scalar, the home range level.
#' @rdname hr
hr_area.RasterLayer <- function(x, level = 0.95, ...) {
    x <- cumulative_ud(x)
    sum(x[] <= level) * prod(raster::res(x))
}
