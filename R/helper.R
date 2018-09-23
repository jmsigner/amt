#' Converts angles to radians
#'
#' @param x `[numeric]`\cr Angles in degrees.
#' @export
#' @examples
#' as_rad(seq(-180, 180, 30))
#'
#' # The default unit of turning angles is degrees.
#' data(deer)
#' deer %>% steps() %>% mutate(ta_ = as_rad(ta_))

as_rad <- function(x) {
  x * pi/180
}

