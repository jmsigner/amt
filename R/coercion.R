#' Track coercion functions
#'
#' amt provides several coercion functions that operate on `tracks` and `steps`.
#'
#' The following coercion functions are currently provided:
#'  * `as_sp` turns a track into `SpatialPoints`.
#' @param x A `track_xy`.
#' @template dots_none
#' @details
#' @name coercion
#' @export
as_sp <- function(x, ...) {
  UseMethod("as_sp", x)
}

#' @export
#' @rdname coercion
as_sp.track_xy <- function(x, ...) {
  sp::SpatialPoints(
    coords = x[, c("x_", "y_")],
    proj4string = if (!is.null(attributes(x)$crs_)) {
      attributes(x)$crs_
    } else {
      sp::CRS(as.character(NA))
    }
  )
}

#' @export
#' @rdname coercion
as_move <- function(x, ...) {
  UseMethod("as_move", x)
}

#' @export
#' @rdname coercion
as_move.track_xy <- function(x, ...) {
  move::move(x = x$x_, y = x$y_, proj = get_crs(x), ...)
}

#' @export
#' @rdname coercion
as_move.track_xyt <- function(x, ...) {
  move::move(x = x$x_, y = x$y_, time = x$t_, proj = get_crs(x), ...)
}
