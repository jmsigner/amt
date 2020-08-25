#' @export
#' @rdname hr
hr_area <- function(x, ...) {
  UseMethod("hr_area", x)
}

#' @export
#' @rdname hr
hr_area.hr <- function(x, units = FALSE, ...) {
  xx <- tibble::as_tibble(sf::st_set_geometry(hr_isopleths(x), NULL))
  if (!units) {
    xx$area <- as.numeric(xx$area)
  }
  xx
}


#' @export
hr_area.RasterLayer <- function(x, level = 0.95, ...) {
    x <- hr_cud(x)
    sum(x[] <= level) * prod(raster::res(x))
}

