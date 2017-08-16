#' Converts angles to radians
#'
#' @param x `[numeric]`\cr Angles.
#' @export
#' @examples
#' as_rad(seq(-180, 180, 30))

as_rad <- function(x) {
  x * pi/180
}
