#' @rdname hr
#' @export
hr_mcp <- function(x, ...) {
  UseMethod("hr_mcp", x)
}

#' @export
#' @rdname hr
hr_mcp.track_xy <- function(x, levels = 0.95, keep.data = TRUE, ...) {

  # check levels
  checkmate::assert_numeric(levels, lower = 0, upper = 1, min.len = 1)
  checkmate::assert_logical(keep.data, len = 1)
  levels <- sort(levels)

  xy <- x[, c("x_", "y_")]
  mxy <- colMeans(xy)
  sqd <- (xy$x_ - mxy[1])^2 + (xy$y_ - mxy[2])^2
  qts <- stats::quantile(sqd, levels)
  geometry <- lapply(qts, function(i) chull_mcp(xy[sqd <= i, ]))
  geometry <- sf::st_as_sfc(geometry, crs = as.character(get_crs(x)))
  mcps <- sf::st_sf(level = levels, area = sf::st_area(geometry), geometry)
  mcp <- list(mcp = mcps, levels = levels, estimator = "mcp",
              crs = get_crs(x),
              data = if (keep.data) x else NULL)
  class(mcp) <- c("mcp", "hr_geom", "hr")
  mcp
}

chull_mcp <- function(x) {
  x <- as.matrix(x)
  ch <- grDevices::chull(x)
  ch <- c(ch, ch[1])
  sf::st_polygon(list(x[ch, 1:2]))
}


