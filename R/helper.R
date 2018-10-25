#' Converts angles to radians
#'
#' @param x `[numeric]`\cr Angles in degrees or rad.
#' @export
#' @name convert_angles
#' @examples
#' as_rad(seq(-180, 180, 30))
#'
#' # The default unit of turning angles is rad.
#' data(deer)
#' deer %>% steps() %>% mutate(ta_ = as_degree(ta_))

as_rad <- function(x) {
  x * pi / 180
}

#' @rdname convert_angles
#' @export
as_degree <- function(x) {
  x * 180 / pi
}
