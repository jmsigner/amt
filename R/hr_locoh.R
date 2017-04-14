#' Local Convex Hull
#'
#' Calculate Local Convex Hull (LoCoH) home range type "k". That is, the k nearest neighbours are considered.
#'
#' @param x A `track_xy`.
#' @param n Numeric scalar, the number of neighbours.
#' @param level Numeric scalar/vector, home-range level. The level should be `0 < level < 1`.
#' @param rand_buffer Numeric scalar, random buffer to avaoid polygons with area 0 (if coordinates are numerically identical).
#' @template dots_none
#' @name locoh_k
#' @export
locoh_k <- function(x, ...) {
  UseMethod("locoh_k", x)
}

#' @export
#' @rdname locoh_k
#' @examples
#' data(sh)
#' x <- track(x = sh[, 1], y = sh[, 2])
#' l <- locoh_k(x)
locoh_k.track_xy <- function(x, n = 10, level = 0.95, rand_buffer = 1e-5, ...) {

  aa <- FNN::get.knn(x[, c("x_", "y_")], k = n)$nn.index
  xysp <- sp::SpatialPointsDataFrame(x[, c("x_", "y_")], data=data.frame(id=1:nrow(x)))

  zz <- lapply(1:nrow(aa), function(x) xysp[aa[x, ], ])
  mcps <- lapply(zz, function(x) rgeos::gBuffer(rgeos::gConvexHull(x), width = rand_buffer))

  mcpAreas <- sapply(mcps, rgeos::gArea)

  mcpAreasOrder <- order(mcpAreas)

  ff <- zz[mcpAreasOrder]
  mm <- mcps[mcpAreasOrder]


  pp <- rep(NA_integer_, length(ff))
  seen <- c()

  ## this is still slow
  for (i in 1:length(ff)) {
    seen <- union(seen, ff[[i]]$id)
    pp[i] <- length(unique(seen))
  }

  qq <- list()
  qq[[1]] <- mm[[1]]
  pp <- pp/nrow(x)

  wlevel <- sapply(level, function(l) which.min(abs(pp - l)))
  for (i in seq_along(wlevel)) {
    ## buffer is necessary, to overcome some topology errors if the polygon is quasi a line
    p1 <- lapply(1:wlevel[i], function(i) sp::Polygon(mm[[i]]@polygons[[1]]@Polygons[[1]]@coords))
    ff <- sp::SpatialPolygons(list(sp::Polygons(p1, ID=1)))

    qq[[i]] <- rgeos::gBuffer(rgeos::gUnaryUnion(ff), width=0, id=i)
  }

  rr <- do.call(sp::rbind.SpatialPolygons, qq)
  areas <- sapply(qq, rgeos::gArea)

  qq2 <- sp::SpatialPolygonsDataFrame(rr, data=data.frame(level=round(pp[wlevel], 2),
                                                          area=areas), match.ID=FALSE)

  if (!is.null(attr(x, "crs_"))) {
    sp::proj4string(qq2) <- attr(x, "crs_")
  }

  qq2

##   head(bb)
##
##   mcps2 <- lapply(1:nrow(bb), function(i) {
##     xx <- xy[bb[i, ], ]
##     ch <- chull(xx)
##     xxc <- xx[c(ch, ch[1]), ]
##   })
##
##   areas <- sapply(mcps2, function(x) {
##     n <- nrow(x)
##     x <- x[n:1, ]
##     # follow: http://www.mathwords.com/a/area_convex_polygon.htm
##     0.5 * (sum(as.numeric(x[-n, 1]) * x[-1, 2]) - sum(as.numeric(x[-1, 1]) * x[-n, 2]))
##   })
##
##   mcp_areas_order <- order(areas)
##
##   ff <- areas[mcp_areas_order]
##   ff_ids <- mcp_areas_order
##   mm <- mcps2[mcp_areas_order]
##
##   pp <- rep(NA_integer_, length(ff))
##   seen <- c()
##
##   ## this is still slow
##   for (i in 1:length(ff)) {
##     pp[i] <- length(unique(as.vector(bb[1:i, ])))
##   }
##
##   qq <- list()
##   qq[[1]] <- mm[[1]]
##   pp <- pp/nrow(xy) * 100
##
##   level <- 95
##
##   wlevel <- sapply(level, function(l) which.min(abs(pp - l)))
##   for (i in seq_along(wlevel)) {
##     ## buffer is necessary, to overcome some topology errors if the polygon is quasi a line
##     p1 <- lapply(1:wlevel[i], function(i) sp::Polygon(mm[[i]]))
##     ff <- sp::SpatialPolygons(list(sp::Polygons(p1, ID=1)))
##     qq[[i]] <- rgeos::gBuffer(rgeos::gUnaryUnion(ff), width=0, id=i)
##   }
##
##   rr <- do.call(rbind, qq)
##   areas <- sapply(qq, rgeos::gArea)
##
##   sp::SpatialPolygonsDataFrame(rr, data=data.frame(level=round(pp[wlevel], 2),
##                                                    area=areas), match.ID=FALSE)
##
}
