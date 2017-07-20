#' Inspect a track
#'
#' Provides a very basic interface to `leaflet` and lets the user inspect relocations on an interactive map.
#'
#' @template track_xy_star
#' @param cluster `[logical(1)]` \cr If `TRUE` points are clustered at lower zoom levels.
#' @param popup `[character(nrow(x))]` \cr Optional labels for popups.
#' @template dots_none
#' @name inspect
#' @note Important, `x` requires a valid coordinate reference system.
#' @export
#' @return An interactive `leaflet` map.
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
  if (has_crs(x)) {
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
