#' Calculate a cumulative UD
#'
#' Calculate the cumulative utilization distribution (UD).
#'
#' @param x `[RasterLayer]` \cr Containing the Utilization Distribution (UD).
#' @template dots_none
#' @export
#' @note This function is typically used to obtain isopleths.
#' @return `[RasterLayer]` \cr The cumulative UD.
#' @name cum_ud
cumulative_ud <- function (x, ...) {
  UseMethod("cumulative_ud", x)
}

#' @export
#' @rdname cum_ud
cumulative_ud.RasterLayer <- function(x, ...) {
  r1 <- x
  v <- raster::getValues(r1)
  v <- v / sum(v, na.rm = TRUE)  # standarize
  udFromDat <- raster::setValues(r1, v)
  v <- cumsum(v[order(-v)])[order(order(-v))]
  raster::setValues(r1, v)
}
