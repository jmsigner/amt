#' @export
plot.hr <- function(x, ...) {
    plot(sf::st_geometry(hr_isopleths(x)), ...)
  if (!is.null(x$data)) {
    points(x$data$x_, x$data$y_, col = grDevices::adjustcolor("black", 0.2),
           pch = 20)
  }
}
