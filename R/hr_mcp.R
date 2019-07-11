#' @rdname hr
#' @export
hr_mcp <- function(x, ...) {
  UseMethod("hr_mcp", x)
}

#' @export
#' @rdname hr
hr_mcp.track_xy <- function(x, levels = 0.95, ...) {

  # check levels
  checkmate::assert_numeric(levels, lower = 0, upper = 1, min.len = 1)

  xy <- x[, c("x_", "y_")]
  mxy <- colMeans(xy)
  sqd <- (xy$x_ - mxy[1])^2 + (xy$y_ - mxy[2])^2
  qts <- stats::quantile(sqd, levels)
  mcps <- lapply(qts, function(i) chull_mcp(xy[sqd <= i, ]))
  class(get_crs(x))
  mcps <- sf::st_as_sfc(mcps, crs = as.character(get_crs(x)))
  mcps <- sf::st_sf(mcps, area = sf::st_area(mcps))
  mcp <- list(mcp = mcps)
  class(mcp) <- c("mcp", "hr")
  mcp
}




# @param x A data.frame or matrix with the x and y coords
# @param l id for the resulting poly
# @return Polygon
chull_mcp <- function(x) {
  x <- as.matrix(x)
  ch <- grDevices::chull(x)
  ch <- c(ch, ch[1])
  sf::st_polygon(list(x[ch, 1:2]))
}


