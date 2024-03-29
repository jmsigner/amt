#' @rdname hrest
#' @param type `k`, `r` or `a`. Type of LoCoH.
#' @export
hr_locoh <- function(x, ...) {
  UseMethod("hr_locoh", x)
}


#' @export
#' @rdname hrest
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
      stop(paste0("hr_locoh, type k, n > number of points"))
    }
    ## 1. calc dist
    ## 2. order by dist
    ## 3. take n nearest
    if (!requireNamespace("FNN", quietly = TRUE)) {
      stop("Please install package `FNN` first.")
    }
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

  zz <- lapply(seq_along(aa), function(.x) {
    zz <- x[c(.x, aa[[.x]]), ]
    zz$id = c(.x, aa[[.x]])
    zz
  })
  names(zz) <- seq_along(zz)
  # data.table would be much faster here, but decided against it, otherwise would have another dependency.
  # bb <- data.table::rbindlist(zz, idcol = "id")
  bb <- dplyr::bind_rows(zz, .id = "id")
  mp <- sfheaders::sf_multipoint(
    bb, x = "x_", y = "y_", multipoint_id = "id")
  mcps <- sf::st_convex_hull(mp) |> sf::st_buffer(dist = rand_buffer)
  mcpAreas <- sf::st_area(mcps)

  mcpAreasOrder <- order(mcpAreas)

  ff <- zz[mcpAreasOrder]
  mm <- mcps[mcpAreasOrder, ]

  ## Compute vector `pp`, which records what proportion of points,
  ## cumulatively, have been "seen" with introduction of the nth
  ## smallest polygon.
  X <- lapply(ff, "[[", "id")
  all_polys <- seq_along(X)
  id_poly <- rep(all_polys, lengths(X))
  id_pt <- unlist(X)
  ## Vector with indices of polygon in which nth point first "seen"
  id_poly <- id_poly[!(duplicated(id_pt))]
  pp <- findInterval(all_polys, id_poly)
  pp <- pp/nrow(x)

  qq <- list()
  qq[[1]] <- mm[[1]]

  wlevel <- sapply(levels, function(l) which.min(abs(pp - l)))
  for (i in seq_along(wlevel)) {
    qq[[i]] <- sf::st_union(mm[1:wlevel[i], ], by_feature = FALSE)
  }


  qq2 <- sf::st_sf(
    level = round(pp[wlevel], 2),
    what = "estimate",
    geometry = sf::st_sfc(unlist(qq, recursive = FALSE))
  )

  # Add CRS
  if (!is.null(attr(x, "crs_"))) {
    sf::st_crs(qq2) <- sf::st_crs(attr(x, "crs_"))
  } else {
    sf::st_crs(rr) <- get_crs(x)
  }

  # Add area
  qq2$area <- sf::st_area(qq2)

  out <- list(locoh = qq2[, c("level", "what", "area", "geometry")],
              levels = levels, type = type, n = n, estimator = "locoh",
              crs = get_crs(x),
              data = if (keep.data) x else NULL)
  class(out) <- c("locoh", "hr_geom", "hr", "list")
  out

}

