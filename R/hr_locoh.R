#' @rdname hr
#' @export
hr_locoh_k <- function(x, ...) {
  UseMethod("hr_locoh_k", x)
}


#' @export
#' @rdname hr
hr_locoh_k.track_xy <- function(x, n = 10, levels = 0.95, rand_buffer = 1e-5, ...) {

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

  wlevel <- sapply(levels, function(l) which.min(abs(pp - l)))
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

  out <- list(locoh = qq2)
  class(out) <- c("locoh", "hr", "list")
  out

}

