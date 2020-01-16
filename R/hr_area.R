#' @export
#' @rdname hr
hr_area <- function(x, ...) {
  UseMethod("hr_area", x)
}

#' @export
hr_area.mcp <- function(x, ...) {
  as_tibble(x$mcp)
}

#' @export
hr_area.locoh <- function(x, ...) {
  as_tibble(x$locoh)
}

#' @export
hr_area.RasterLayer <- function(x, level = 0.95, ...) {
    x <- cumulative_ud(x)
    sum(x[] <= level) * prod(raster::res(x))
}

#' @export
hr_area.kde <- function(x, level = 0.95, ...) {
  hr_area(x$ud, level = level, ...)
}


#' @export
hr_area.akde <- function(x, level = 0.95, ...) {
  hr_area(x$ud, level = level, ...)
}
