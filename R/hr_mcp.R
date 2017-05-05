#' Minimum Convex Polygon
#'
#' Function to calcualte a Minimum Convex Polygon (MCP) home range.
#' @param x A track.
#' @param levels, the home range levels.
#' @template dots_none
#' @name mcp
#' @export
#' @examples
#' data(sh)
#' trk <- track(sh$x_, sh$y_)
#' mcps <- mcp(trk, levels = c(0.5, 0.95, 1))
#' \dontrun{
#' plot(trk, asp = 1)
#' sp::plot(mcps$mcp, add = TRUE)
#' }
#'
#'
mcp <- function(x, levels = 0.95, ...) {
  UseMethod("mcp", x)
}

#' @export
#' @rdname mcp
mcp.track_xy <- function(x, levels = 0.95, ...) {
  xy <- select_(x, ~ x_, ~ y_)
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
