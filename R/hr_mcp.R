#' @rdname hr
#' @export
hr_mcp <- function(x, ...) {
  UseMethod("hr_mcp", x)
}

#' @export
#' @rdname hr
hr_mcp.track_xy <- function(x, levels = 0.95, ...) {
  xy <- select(x, c("x_", "y_"))
  mxy <- colMeans(xy)
  sqd <- (xy$x_ - mxy[1])^2 + (xy$y_ - mxy[2])^2
  qts <- stats::quantile(sqd, levels)
  mcps <- lapply(qts, function(i) chull_mcp(xy[sqd <= i, ]))

  for (i in seq_along(mcps)) {
    mcps[[i]] <- sp::spChFIDs(mcps[[i]], as.character(levels[i]))
  }

  mcps <- do.call(sp::rbind.SpatialPolygons, mcps)
  mcps <- sp::SpatialPolygonsDataFrame(mcps, data.frame(level=names(mcps), area=rgeos::gArea(mcps, byid=TRUE)))
  mcp <- list(mcp = mcps)

  if (has_crs(x)) {
    sp::proj4string(mcp$mcp) <- get_crs(x)
  }

  class(mcp) <- c("mcp", "hr")
  mcp
}




# @param x A data.frame or matrix with the x and y coords
# @param l id for the resulting poly
# @return SpatialPolygons
chull_mcp <- function(x, l = 1) {
  ch <- grDevices::chull(x)
  ch <- c(ch, ch[1])
  sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(x[ch, 1:2], hole = FALSE)), ID = l)))
}
