#' @keywords internal
validate_covars <- function(x) {
  if (!is(x, "SpatRaster")) {
    x <- raster_to_terra(x)
  }
  checkmate::assert_class(x, "SpatRaster")
}



#' @keywords internal
validate_coords <- function(x, ...) {
  UseMethod("validate_coords", x)
}


#' @keywords internal
validate_coords.track_xy <- function(x, allow.na = FALSE) {
  checkmate::assert_numeric(x$x_, any.missing = allow.na)
  checkmate::assert_numeric(x$y_, any.missing = allow.na)
  TRUE
}

#' @keywords internal
validate_coords.track_xyt <- function(x, allow.na = FALSE) {
  checkmate::assert_numeric(x$x_, any.missing = allow.na)
  checkmate::assert_numeric(x$y_, any.missing = allow.na)
  TRUE
}

#' @keywords internal
validate_coords.steps_xy <- function(x, allow.na = FALSE) {
  checkmate::assert_numeric(x$x1_, any.missing = allow.na)
  checkmate::assert_numeric(x$x2_, any.missing = allow.na)
  checkmate::assert_numeric(x$y1_, any.missing = allow.na)
  checkmate::assert_numeric(x$y2_, any.missing = allow.na)
  TRUE
}
