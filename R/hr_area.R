#' @export
#' @rdname hr
hr_area <- function(x, ...) {
  UseMethod("hr_area", x)
}

#' @export
hr_area.hr <- function(x, ...) {
  tibble::as_tibble(sf::st_set_geometry(hr_isopleths(x), NULL))
}


#' @export
hr_area.RasterLayer <- function(x, level = 0.95, ...) {
    x <- cumulative_ud(x)
    sum(x[] <= level) * prod(raster::res(x))
}

