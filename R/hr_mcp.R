#' Minimum Convex Polygon
#'
#' Function to calcualte a Minimum Convex Polygon (MCP) home range.
#' @param x A track.
#' @param levels, the home range levels.
#' @template dots_none
#' @name mcp
#' @export
mcp <- function(x, levels = 0.95, ...) {
  UseMethod("mcp", x)
}

#' @export
#' @rdname mcp
mcp.track_xy <- function(x, levels = 0.95, ...) {
    # dist to centroid
    cent <- centroid(x)
    x$dist = sqrt((x$x_ - cent[1])^2 + (x$y_ - cent[2])^2)

    q <- quantile(x$dist, c(0.95))
    mcps <- lapply(seq_along(q), function(i) rgeos::gConvexHull(as_sp(x[x$dist <= q[i], ]), id = q[i]))

    # Merge isopleths
    bb <- do.call(sp::rbind.SpatialPolygons, mcps)
    bb <- sp::SpatialPolygonsDataFrame(bb, data.frame(level=names(bb), area=rgeos::gArea(bb, byid=TRUE)))
    bb <- list(mcp = bb)
    class(bb) <- c("mcp", "hr")
    bb
}



# Faster, but with an error -----------------------------------------------

mcp2 <- function(x, levels = 95) {
  UseMethod("mcp2", x)
}

mcp2.track_xy <- function(x, levels = 0.95) {
    xy <- select_(x, ~ x_, ~ y_)
    mxy <- colMeans(xy)
    sqd <- (x$x_ - mxy[1])^2 + (x$y_ - mxy[2])^2
    qts <- stats::quantile(sqd, levels)

    mcp <- map(qts, ~ filter_(xy, ~ sqd <= .) %>% grDevices::chull(.)) %>%
                  map(., ~ sp::SpatialPoints(as.matrix(xy[., ])), xy) %>%
                  map(., ~ rgeos::gConvexHull(.))

    for (i in seq_along(mcp)) {
      mcp[[i]] <- sp::spChFIDs(mcp[[i]], as.character(levels[i]))
    }

    mcp <- do.call(sp::rbind.SpatialPolygons, mcp)
    mcp <- sp::SpatialPolygonsDataFrame(mcp, data.frame(level=names(mcp), area=rgeos::gArea(mcp, byid=TRUE)))
    mcp <- list(mcp = mcp)
    class(mcp) <- c("mcp2", "hr")
    mcp
}
