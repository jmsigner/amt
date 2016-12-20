#' @export
#'
#'
mcp <- function(x, levels = 95) {
  if (check_trackXY(x)) {
    xy <- spbabel::sp(x)

    # dist to centroid
    dists <- data.frame(
      id=1:length(xy),
      dist=as.vector(rgeos::gDistance(rgeos::gCentroid(xy), xy, byid=TRUE)))

    # calculate mcps
    mcps <- lapply(levels, function(l) rgeos::gConvexHull(xy[dists[dists$dist <= quantile(dists$dist, l/100), "id"], ], id=l))

    # Merge isopleths
    bb <- do.call(sp::rbind.SpatialPolygons, mcps)
    bb <- sp::SpatialPolygonsDataFrame(bb, data.frame(level=names(bb), area=rgeos::gArea(bb, byid=TRUE)))
    bb <- list(mcp = bb)
    class(bb) <- c("amt_mcp", "amt_hr")
    bb
  }
}

#' @export
mcp2 <- function(x, levels = 95) {
  if (check_trackXY(x)) {
    xy <- select_(x, ~ x_, ~ y_)
    mxy <- colMeans(xy)
    sqd <- (x$x_ - mxy[1])^2 + (x$y_ - mxy[2])^2
    qts <- stats::quantile(sqd, levels / 100)

    mcp <- map(qts, ~ filter_(xy, ~ sqd <= .) %>% grDevices::chull(.)) %>%
                  map(., ~ sp::SpatialPoints(as.matrix(xy[., ])), xy) %>%
                  map(., ~ rgeos::gConvexHull(.))

    for (i in seq_along(mcp)) {
      mcp[[i]] <- sp::spChFIDs(mcp[[i]], as.character(levels[i]))
    }

    mcp <- do.call(sp::rbind.SpatialPolygons, mcp)
    mcp <- sp::SpatialPolygonsDataFrame(mcp, data.frame(level=names(mcp), area=rgeos::gArea(mcp, byid=TRUE)))
    mcp <- list(mcp = mcp)
    class(mcp) <- c("amt_mcp", "amt_hr")
    mcp
  }
}
