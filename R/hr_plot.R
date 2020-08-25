#' @param add.relocations `logical(1)` indicates if a relocations should be
#'   added to the plot.
#' @export
#'
plot.hr <- function(x, add.relocations = TRUE, ...) {
    plot(sf::st_geometry(hr_isopleths(x)), ...)
  if (!is.null(x$data) & add.relocations) {
    points(x$data$x_, x$data$y_, col = grDevices::adjustcolor("black", 0.2),
           pch = 20)
  }
}
