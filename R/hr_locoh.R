#' @rdname hr
#' @export
hr_locoh <- function(x, ...) {
  UseMethod("hr_locoh", x)
}


#' @export
#' @param type `k`, `r` or `a`. Type of LoCoH.
#' @rdname hr
hr_locoh.track_xy <- function(x, n = 10, type = "k", levels = 0.95, keep.data = TRUE, rand_buffer = 1e-5, ...) {

  ## type
  if (!type %in% c("a", "k", "r")) {
    stop(paste0("hr_loch: incorrect type"))
  }

  n <- as.numeric(n)
  if (is.na(n)) {
    stop(paste("hr_locoh: n should be numeric, not ", n))
  }

  no <- 1:nrow(x)
  if (type == "k") {
    if (n > nrow(x)) {
      n <- nrow(x)
      warning(paste0("hr_locoh, type k, n > number of points, set n to number of points (", n, ")"))
    }
    ## 1. calc dist
    ## 2. order by dist
    ## 3. take n nearest
    aa <- FNN::get.knn(x[, c("x_", "y_")], k = n)$nn.index
    #"r" and "a" methods return a list -- split so that this is the same
    aa <- split(aa, 1:nrow(aa))
  } else if (type == "r") {
    ## 1. calc dist
    ## 2. take all pts with dist <= n
    aa <- lapply(no, function(i)
      no[sqrt((x$x_ - x$x_[i])^2 + (x$y_ - x$y_[i])^2) <= n])
  } else if (type == "a") {
    # 1. calc dist
    # 2. order by dist
    # 3. take cum dist
    # 4. take points where cumist <= n
    aa <- lapply(no, function(i) {
      di <- sqrt((x$x_ - x$x_[i])^2 + (x$y_ - x$y_[i])^2)
      no[order(di)][cumsum(di[order(di)]) <= n]
    })
  }

  xysp <- sp::SpatialPointsDataFrame(x[, c("x_", "y_")], data=data.frame(id=1:nrow(x)))
  zz <- lapply(1:length(aa), function(i) xysp[aa[[i]], ])
  mcps <- lapply(zz, function(x) rgeos::gBuffer(rgeos::gConvexHull(x), width = rand_buffer))
  mcpAreas <- sapply(mcps, rgeos::gArea)


  #With SF
  # Slower, so stay with sp
  #  xysp <- sf::st_as_sf(x, coords = c("x_", "y_"))
  #  mcps <- mutate(xysp,
  #    mcps =  purrr::map(aa, ~  sf::st_buffer(sf::st_convex_hull(sf::st_union(xysp[.x, ])),
  #                                            dist = rand_buffer)))
  #  mcps1 <- mutate(mcps, area = purrr::map_dbl(mcps, sf::st_area))

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

  qq2 <- sf::st_as_sf(qq2)
  qq2$area <- sf::st_area(qq2)

  out <- list(locoh = qq2, levels = levels, type = type, n = n, estimator = "locoh",
              crs = get_crs(x),
              data = if (keep.data) x else NULL)
  class(out) <- c("locoh", "hr_geom", "hr", "list")
  out

}

