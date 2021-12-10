#' Obtain the utilization distribution of a probabilistic home range
#'
#' @param x `[hr_prob]` The home-range estimate
#' @template dots_none
#' @return `RasterLayer`

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

