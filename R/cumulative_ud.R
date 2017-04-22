#' Calculate a cumulative UD
#'
#' @param x A UD.
#' @template dots_none
#' @export
cumulative_ud <- function (x, ...) {
  UseMethod("cumulative_ud", x)
}

#' @export
cumulative_ud.RasterLayer <- function(x, ...) {
  r1 <- x
  v <- raster::getValues(r1)
  v <- v / sum(v, na.rm=TRUE)  # standarize
  udFromDat <- raster::setValues(r1, v)
  v <- cumsum(v[order(-v)])[order(order(-v))]
  raster::setValues(r1, v)
}
