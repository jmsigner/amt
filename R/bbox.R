#' Get bounding box of a track.
#' @template track_xy_star
#' @param spatial `[logical(1)=FALSE]` \cr Whether or not to return a `SpatialPolygons`-object or not.
#' @param buffer `[numeric(0)=NULL]{NULL, >0}` \cr An optional buffer of the bounding box.
#' @param sf `[logical(1)=FALSE]` \cr If `TRUE` a simple feature polygon is returned.
#' @template dots_none
#' @name bbox
#' @export
#' @examples
#' data(deer)
#' bbox(deer)
#' bbox(deer, spatial = FALSE)
#' bbox(deer, buffer = 100, spatial = FALSE)
#'
#' # For steps
#' deer %>% steps_by_burst %>% bbox(spatial = FALSE)
#' deer %>% steps_by_burst %>% bbox(buffer = 100, spatial = FALSE)
#' deer %>% steps_by_burst %>% random_steps %>% bbox(spatial = FALSE)

bbox <- function(x, ...) {
  UseMethod("bbox", x)
}

#' @export
#' @rdname bbox
bbox.track_xy <- function(x, spatial = TRUE, buffer = NULL, sf = FALSE, ...) {
  bbx <- c(min(x$x_), max(x$x_), min(x$y_), max(x$y_))
  bbox_base(bbx, spatial, buffer, sf, x)
}

#' @export
#' @rdname bbox
bbox.steps_xy <- function(x, spatial = TRUE, buffer = NULL, sf = FALSE, ...) {
  bbx <- c(min(c(min(x$x1_), min(x$x2_))),
           max(c(max(x$x1_), max(x$x2_))),
           min(c(min(x$y1_), min(x$y2_))),
           max(c(max(x$y1_), max(x$y2_))))
  bbox_base(bbx, spatial, buffer, sf, x)
}

bbox_base <- function(bbx, spatial, buffer, sf, x) {
  if (sf && !spatial) {
    warning("spatial has precedence over sf.")
  }

  # bounds
  if (!is.null(buffer)) {
    bbx <- bbx + buffer * c(-1, 1, -1, 1)
  }

  coords <- cbind(
    x = c(bbx[1], bbx[1], bbx[2], bbx[2], bbx[1]),
    y = c(bbx[3], bbx[4], bbx[4], bbx[3], bbx[3])
  )

  p <- sf::st_sfc(sf::st_polygon(list(coords)))

  if (has_crs(x)) {
    p <- sf::st_set_crs(p, as.character(get_crs(x)))
  }

  if (spatial) {
    if (sf) {
      p
    } else {
      sf::as_Spatial(p)
    }
  } else {
    sf::st_bbox(p)
  }
}
