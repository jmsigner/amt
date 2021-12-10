#' Transform CRS
#'
#' Transforms the CRS for a track.
#' @template track_xy_star
#' @template dots_none
#' @return A track with transformed coordinates.
#'
#' @export
#' @seealso `sf::st_transform`
#' @name transform_coords

transform_coords <- function(x, ...) {
  UseMethod("transform_coords", x)
}

#' @param crs_from `[crs(1)]` \cr Coordinate reference system the data are currently in, see \code{sf::sf_crs}. If `crs_from` is missing, the `crs`-attribute of the track is used.
#' @param crs_to `[crs(1)]` \cr Coordinate reference system the data should be transformed to, see \code{sf::st_crs}.
#' @export
#' @rdname transform_coords
#' @examples
#' data(deer)
#' get_crs(deer)
#'
#' # project to geographical coordinates (note the CRS is taken automatically from the object deer).
#' d1 <- transform_coords(deer, crs_to = 4326)

transform_coords.track_xy <- function(x, crs_to, crs_from, ...) {

  if (missing(crs_from)) {
    if (!is.null(attributes(x)$crs)) {
      crs_from <- attributes(x)$crs
    }
  }

  # crs_to
  if (!is.na(crs_to)) {
    if (is(crs_to, "CRS")) {
      .Deprecated("It looks like you used `CRS()` to create the crs_to,
                  please use the ESPG directly.")
      crs <- sf::st_crs(crs_to)
    } else {
      crs <- sf::st_crs(crs_to)
      if (is.na(crs_to)) {
        warning("`crs_to` invalid.")
      }
    }
  }

  # crs_from
  if (!is.na(crs_from)) {
    if (is(crs_from, "CRS")) {
      .Deprecated("It looks like you used `CRS()` to create the crs_from,
                  please use the ESPG directly.")
      crs <- sf::st_crs(crs_from)
    } else {
      crs <- sf::st_crs(crs_from)
      if (is.na(crs_from)) {
        warning("`crs_from` invalid.")
      }
    }
  }

  if (!is(crs_from, "crs") && is(crs_to, "crs")) {
    stop("Either crs_from or crs_to not of class `crs`.")
  }

  x_sp <- as_sf_points(x)
  x_sp <- sf::st_coordinates(sf::st_transform(x_sp, crs_to))
  x$x_ <- x_sp[, 1]
  x$y_ <- x_sp[, 2]
  attributes(x)$crs_ <- crs_to
  x
}

#' @rdname transform_coords
transform_crs <- transform_coords
