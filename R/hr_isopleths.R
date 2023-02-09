#' Home-range isopleths
#'
#' Obtain the isopleths of a home-range estimate, possible at different isopleth levels.
#' @param x An object of class `hr`
#' @param levels `[numeric]` \cr The isopleth levels used for calculating home
#'   ranges. Should be `0 < level < 1`.
#' @param conf.level The confidence level for isopleths for `aKDE`.
#' @param descending `[logical = TRUE]` \cr Indicating if levels (and thus the polygons) should be ordered in descending (default) or not.
#' @return A `tibble` with the home-range level and a simple feature columns with the isoploth as multipolygon.
#' @template dots_none
#' @name hr_isopleths
#' @export
hr_isopleths <- function (x, ...) {
  UseMethod("hr_isopleths", x)
}

#' @export
#' @rdname hr_isopleths
hr_isopleths.PackedSpatRaster <- function (x, levels, descending = TRUE, ...) {
  hr_isopleths(terra::unwrap(x), levels = levels, descending = descending, ...)
}

#' @export
#' @rdname hr_isopleths
hr_isopleths.SpatRaster <- function (x, levels, descending = TRUE, ...) {

  con <- terra::as.contour(hr_cud(x), levels = levels)
  con <- sf::st_as_sf(con)
  suppressWarnings(con <- lapply(split(con, con$level), function(l) {
    l |> sf::st_cast("LINESTRING") |> sf::st_cast("POLYGON") |>
      sf::st_union() |> sf::st_cast("MULTIPOLYGON")
  }
  ))
  con <- do.call(c, con)
  con <- sf::st_as_sf(con) |> dplyr::rename(geometry = x) |>
    dplyr::mutate(level = levels)

  # Add area
  con$area <- sf::st_area(con)
  con$what <- "estimate"
  con <- con[, c("level", "what", "area", "geometry")]

  # Set projection
  sf::st_crs(con) <- if (is.null(attr(x, "crs_"))) {
    terra::crs(x)
  } else {
    attr(x, "crs_")
  }

  if (descending) {
    con[order(con$level, decreasing = TRUE), ]
  }

  con
}

#' @export
#' @rdname hr_isopleths
hr_isopleths.mcp <- function (x, descending = TRUE, ...) {
  x$mcp[order(x$mcp$level, decreasing = descending), ]
}

#' @export
#' @rdname hr_isopleths
hr_isopleths.locoh <- function (x, descending = TRUE, ...) {
  x$locoh[order(x$locoh$level, decreasing = descending), ]
}

#' @export
#' @rdname hr_isopleths
hr_isopleths.hr_prob <- function(x, descending = TRUE, ...) {
  iso <- hr_isopleths(x$ud, level = x$levels, ...)
  iso[order(iso$level, decreasing = descending), ]
}

#' @export
#' @rdname hr_isopleths
hr_isopleths.akde <- function(x, conf.level = 0.95, descending = TRUE, ...) {

  checkmate::assert_number(conf.level, lower = 0, upper = 1)
  res <- ctmm::SpatialPolygonsDataFrame.UD(x$akde, level.UD = x$levels,
                                           conf.level = conf.level)
  res1 <- sf::st_as_sf(res)
  res1 <- sf::st_transform(res1, x$crs)
  res1 <- res1[, setdiff(names(res1), "name")]
  res1$level <- rep(x$levels, each = nrow(res1) / 2)
  res1$what <- rep(c(paste0("lci (", conf.level, ")"),
                     "estimate",
                     paste0("uci (", conf.level,")")),
                   length(x$levels))
  res1$area = sf::st_area(res1)
  res1 <- res1[, c("level", "what", "area", "geometry")]
  res1[order(res1$level, decreasing = descending), ]
}
