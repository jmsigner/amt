#' @export
#' @rdname hr
#' @param unit `[logic(1)]` \cr Should areas be returned as units? If `FALSE`
#'   areas are returned as numeric values.
hr_area <- function(x, units = FALSE, ...) {
  UseMethod("hr_area", x)
}

#' @export
hr_area.hr <- function(x, units = FALSE, ...) {
  xx <- tibble::as_tibble(sf::st_set_geometry(hr_isopleths(x), NULL))
  if (!units) {
    xx$area <- as.numeric(xx$area)
  }
  xx
}


#' @export
hr_area.RasterLayer <- function(x, level = 0.95, ...) {
    x <- cumulative_ud(x)
    sum(x[] <= level) * prod(raster::res(x))
}

