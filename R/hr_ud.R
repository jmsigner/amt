#' Obtain the utilization distribution of a probabilistic home-range estimate
#'
#' @param x `[hr_prob]` The home-range estimate
#' @template dots_none
#' @return `SpatRaster`

#' @export
hr_ud <- function(x, ...) {
  UseMethod("hr_ud", x)
}

#' @export
hr_ud.hr_prob <- function(x, ...) {
  ud <- x$ud
  ud[] <- ud[] / sum(ud[], na.rm = TRUE)
  ud
}

