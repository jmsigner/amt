#' Home-range isopleths
#'
#' Obtain the isopleths of a home-range estimate, possible at different isopleth levels.
#' @param x An object of class `hr`
#' @param level `[numeric]` \cr The isopleth levels used for calculating home
#'   ranges. Should be `0 < level < 1`.
#' @param conf.level The confidence level for isopleths for `aKDE`.
#' @return A `tibble` with the home-range level and a simple feature columns with the isoploth as multipolygon.
#' @template dots_none
#' @name hr_isopleths
#' @export
hr_isopleths <- function (x, ...) {
  UseMethod("hr_isopleths", x)
}

#' @export
#' @rdname hr_isopleths
hr_isopleths.RasterLayer <- function (x, level, ...) {
  con <- raster::rasterToContour(hr_cud(x), level = level)
  b <- sp::coordinates(con)

  # make sure there are at least 2 points
  b <- lapply(b, function(x) Filter(function(x) nrow(x) > 2, x))

  # Make spatial polyons
  # Complete ring and create each Polygon
  con <- lapply(b, function(x) {
    if (length(x) == 1) {
      lapply(x, function(xx) sp::Polygon(rbind(xx, xx[1,])[, 1:2], hole = FALSE))
    } else {
      bb <- sp::SpatialPolygons(lapply(seq(length(x)), function(i)
        sp::Polygons(list(sp::Polygon(rbind(x[[i]], x[[i]][1, ])[, 1:2])), i)))
      if (any((tm <- rgeos::gIntersects(bb, byid = TRUE))[upper.tri(tm)])) {

        # some polygons intersect find out which and set as wholes
        pos <- expand.grid(b=1:length(bb), s = 1:length(bb))
        holes <- rep(FALSE, length(bb))

        for (i in 1:nrow(pos)) {
          if (rgeos::gContainsProperly(bb[pos[i,1]], bb[pos[i,2]])) {

            # second poly is contained by the first
            holes[pos[i,2]] <- TRUE
          }
        }
        lapply(seq_along(x), function(i)
          sp::Polygon(rbind(x[[i]], x[[i]][1,])[, 1:2], hole=holes[i]))
      } else {
        lapply(x, function(xx) sp::Polygon(rbind(xx, xx[1,])[, 1:2], hole=FALSE))
      }
    }
  })

  # Check holes, if more than 1 poly, make sp polygons, then check wholes
  # create a list of Polygons for each level
  con <- lapply(seq_along(con), function(i) sp::Polygons(con[[i]], i))
  con <- sp::SpatialPolygons(con)

  df <- data.frame(
    level = level,
    what = "estimate",
    area = rgeos::gArea(con, byid = TRUE))
  row.names(df) <- 1:length(level)
  con <- sp::SpatialPolygonsDataFrame(con, df)

  con <- sf::st_as_sf(con)
  con$area <- sf::st_area(con)
  # Set projection
  sf::st_crs(con) <- if (is.null(attr(x, "crs_"))) {
    raster::projection(x)}
  else {
    attr(x, "crs_")
  }
  con

}

#' @export
#' @rdname hr_isopleths
hr_isopleths.mcp <- function (x, ...) {
  x$mcp
}

#' @export
#' @rdname hr_isopleths
hr_isopleths.locoh <- function (x, ...) {
  x$locoh
}

#' @export
#' @rdname hr_isopleths
hr_isopleths.hr_prob <- function(x, ...) {
  iso <- hr_isopleths(x$ud, level = x$levels, ...)
  iso
}

#' @export
#' @rdname hr_isopleths
hr_isopleths.akde <- function(x, conf.level = 0.95, ...) {

  checkmate::assert_number(conf.level, lower = 0, upper = 1)
  res <- ctmm::SpatialPolygonsDataFrame.UD(x$akde, level.UD = x$levels,
                                           conf.level = conf.level)
  res1 <- sf::st_as_sf(res)
  res1 <- sf::st_transform(res1, x$crs)
  res1 <- res1[, setdiff(names(res1), "name")]
  res1$level <- rep(x$levels, each = nrow(res1))
  res1$what <- rep(c(paste0("lci (", conf.level, ")"),
                     "estimate",
                     paste0("uci (", conf.level,")")),
                   length(x$levels))
  res1$area = sf::st_area(res1)
  res1[, c("level", "what", "area", "geometry")]
}
