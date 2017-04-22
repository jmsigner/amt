#' Working with home ranges
#'
#' Function to calculate the area and isopleths of a home range.
#'
#' @param x Fitted home range.
#' @param levels Numeric vector, the home range level (`0 < level < 1` is assumed).
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
#' @rdname hr
hr_area.RasterLayer <- function(x, levels = 0.95, ...) {

  if (length(levels) > 1) {
    warning("only first level is used")
    levels <- levels[1]
  }

  x <- cumulative_ud(x)
  sum(x[] <= level) * prod(raster::res(x))
}
