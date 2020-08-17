#' @rdname hr
#' @export
hr_isopleths <- function (x, ...) {
  UseMethod("hr_isopleths", x)
}

#' @export
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

  ## Set proj4string
  sp::proj4string(con) <- raster::projection(x)

  df <- data.frame(level = level,
                   area = rgeos::gArea(con, byid = TRUE))
  row.names(df) <- 1:length(level)
  con <- sp::SpatialPolygonsDataFrame(con, df)

  con <- sf::st_as_sf(con)
  con$area <- sf::st_area(con)
  con
}

#' @export
hr_isopleths.mcp <- function (x, ...) {
  x$mcp
}

#' @export
hr_isopleths.locoh <- function (x, ...) {
  x$locoh
}

#' @export
hr_isopleths.hr_prob <- function (x, ...) {
  iso <- hr_isopleths(x$ud, level = x$levels, ...)
  iso
}

