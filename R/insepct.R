#' Inspect a track
#'
#' Provides a very basic interfact to `leaflet` and lets the user inspect relocations on an interactive map.
#'
#' @param x A `track_xy`. Important, `x` requires a valid coordinate reference system.
#' @param cluster A locigcal scalar, indicating if points should be clustered at lower zoom levels.
#' @param popup A character vector of length == `nrow(x)`, optionally providing popups.
#' @template dots_none
#' @name inspect
#' @export
#' @seealso `leaflet::leaflet()`
#' @examples
#' data(sh)
#' x <- track(x = sh$x, y = sh$y, crs = sp::CRS("+init=epsg:31467"))
#'
#' \dontrun{
#' inspect(x)
#' inspect(x, cluster = FALSE)
#' inspect(x, popup = 1:nrow(x), cluster = FALSE)
#' }

inspect <- function(x, ...) {
  UseMethod("inspect", x)
}

#' @export
#' @rdname inspect
inspect.track_xy <- function(x, popup = NULL, cluster = TRUE, ...) {
  if (!is.null(attr(x, "crs_"))) {
    x <- transform_coords(x, sp::CRS("+init=epsg:4326"))
  } else {
    stop("x is not projected.")
  }
  leaflet::leaflet(x) %>%
    leaflet::addTiles(group = "OSM (default)") %>%
    leaflet::addProviderTiles(leaflet::providers$OpenSeaMap, group = "OpenSeaMap") %>%
    leaflet::addProviderTiles(leaflet::providers$OpenTopoMap, group = "OpenTopoMap") %>%
    leaflet::addProviderTiles(leaflet::providers$Stamen.Watercolor, group = "Water color") %>%
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "ESRI World Imagery") %>%
    leaflet::addScaleBar() %>%
    leaflet::addCircleMarkers(
      ~x_, ~y_, radius = 7,
      group = "Relocations",
      popup = if (!is.null(popup)) as.character(popup) else NULL,
      clusterOptions = if (cluster) leaflet::markerClusterOptions() else NULL
    ) %>%
    leaflet::addLayersControl(
      baseGroups = c("OSM (default)", "OpenSeaMap", "OpenTopoMap", "Water color", "ESRI WorldImagery"),
      overlayGroups = c("Relocations"),
      options = leaflet::layersControlOptions(collapsed = TRUE)
    )
}
