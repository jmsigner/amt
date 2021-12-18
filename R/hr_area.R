#' Home-range area
#'
#' Obtain the area of a home-range estimate, possible at different isopleth levels.
#' @param x An object of class `hr`
#' @param units `[logic(1)]` \cr Should areas be returned as units? If `FALSE` areas are returned as numeric values.
#' @param level The level at which the area will be calculated.
#' @template dots_none
#' @return A `tibble` with the home-range level and the area.
#' @export
#' @name hr_area
hr_area <- function(x, ...) {
  UseMethod("hr_area", x)
}

#' @export
#' @rdname hr_area
hr_area.hr <- function(x, units = FALSE, ...) {
  xx <- tibble::as_tibble(sf::st_set_geometry(hr_isopleths(x), NULL))
  if (!units) {
    xx$area <- as.numeric(xx$area)
  } else if (units & is.na(x$crs)) {
    warning("Units requested from data that has no CRS assigned. Please assign a CRS first.")
  }
  xx
}


#' @export
#' @rdname hr_area
hr_area.RasterLayer <- function(x, level = 0.95, ...) {
    hr_isopleths(x, level = level)$area
}


#' @export
#' @rdname hr_area
hr_area.akde <- function(x, units = FALSE, ...) {
  xx <- tibble::as_tibble(sf::st_set_geometry(hr_isopleths(x), NULL))
  if (!units) {
    xx$area <- as.numeric(xx$area)
  }
  xx
}
