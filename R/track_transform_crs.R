#' Transform CRS
#'
#' Transforms the CRS for a track.
#' @param x A track.
#' @template dots_none
#'
#' @export
#' @seealso sp::spTransform
#' @name transform_coords
transform_coords <- function(x, ...) {
  UseMethod("transform_coords", x)
}

#' @param crs_from coordinate reference system the data are currently in, see \code{sp::CRS}. If `crs_from` is missing, the `crs`-attribute of the track is used.
#' @param crs_to coordinate reference system the data should be transformed to, see \code{sp::CRS}.
#' @export
#' @rdname transform_coords

transform_coords.track_xy <- function(x, crs_to, crs_from, ...) {
  if (missing(crs_from)) {
    if (!is.null(attributes(x)$crs)) {
      crs_from <- attributes(x)$crs
    }
  }

  if (!is(crs_from, "CRS") && is(crs_to, "CRS")) {
    stop("Either crs_from or crs_to not of class sp::CRS.")
  }

  x_sp <- as_sp(x)
  x_sp <- sp::coordinates(sp::spTransform(x_sp, crs_to))
  x$x_ <- x_sp[, 1]
  x$y_ <- x_sp[, 2]
  attributes(x)$crs_ <- crs_to
  x
}
