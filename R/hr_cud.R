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
hr_cud <- function (x, ...) {
  UseMethod("hr_cud", x)
}

#' @export
#' @rdname cum_ud
hr_cud.SpatRaster <- function(x, ...) {
  r1 <- x
  v <- terra::values(r1)
  v <- v / sum(v, na.rm = TRUE)  # standarize
  v <- cumsum(v[order(-v)])[order(order(-v))]
  terra::values(r1) <- v
  r1
}

#' @export
hr_cud.hr_prob <- function(x, ...) {
  hr_cud(hr_ud(x), ...)
}

