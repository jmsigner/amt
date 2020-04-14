#' @export
plot.hr <- function(x, ...) {
  plot(hr_isopleths(x)[, "level"], col = NA)

}
