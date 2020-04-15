#' @export
plot.hr <- function(x, col = NA, ...) {
  plot(hr_isopleths(x)[, "level"], col = col, ...)

}
