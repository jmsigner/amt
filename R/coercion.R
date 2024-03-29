#' Coerce a track to other formats.
#'
#' Several other packages provides methods to analyze movement data, and `amt` provides coercion methods to some packages.
#'
#' @template track_xy_star
#' @param id `[numeric,character,factor]` \cr Animal id(s).
#' @template dots_none
#' @name coercion
#' @return An object of the class to which coercion is performed to.
#' @export
as_sf <- function(x, ...) {
  UseMethod("as_sf", x)
}

#' @export
as_sf.track_xy <- function(x, ...) {
  as_sf_points(x)
}

#' @export
#' @param end `[logical(1)=TRUE]` \cr For steps, should the end or start points be used?
#' @rdname coercion
as_sf.steps_xy <- function(x, end = TRUE, ...) {
  as_sf_points(x, end = end, ...)
}

#' @export
#' @rdname coercion
as_sp <- function(x, ...) {
  .Deprecated("as_sf")
}

#' Coerces a track to points
#'
#' Coerces a track to points from the `sf` package.
#'
#' @template track_xy_star
#' @template dots_none
#' @name as_sf_points
#' @return A data `data.frame` with a `sfc`-column
#' @export

as_sf_points <- function(x, ...) {
  UseMethod("as_sf_points", x)
}

#' @export
as_sf_points.track_xy <- function(x, ...) {

  p <- sf::st_as_sf(x, coords = c("x_", "y_"))
  p <- sf::st_set_crs(p, if (!is.null(attributes(x)$crs_))
    attributes(x)$crs else sf::NA_crs_)
  p
}

#' @rdname as_sf_points
#' @export
#' @param end `[logical(1)=TRUE]` \cr For steps, should the end or start points be used?
as_sf_points.steps_xy <- function(x, end = TRUE, ...) {

  p <- if (end) {
    sf::st_as_sf(x, coords = c("x2_", "y2_"))
  } else {
    sf::st_as_sf(x, coords = c("x1_", "y1_"))
  }
  p <- sf::st_set_crs(p, if (!is.null(attributes(x)$crs))
    attributes(x)$crs else sf::NA_crs_)
  p
}

# as_sf_lines ----------------------------------------------------------------

#' Export track to lines
#'
#' Exports a track to (multi)lines from the `sf` package.
#'
#' @template track_xy_star
#' @template dots_none
#' @return A `tibble` with a `sfc`-column
#' @export
as_sf_lines <- function(x, ...) {
  UseMethod("as_sf_lines", x)
}

#' @export
as_sf_lines.track_xy <- function(x, ...) {

  # > 1 points
  if (nrow(x) < 2) {
    stop("> 2 locations are required for a line.")
  }

  # bursts
  if ("burst_" %in% names(x)) {
    if (any(table(x$burst_) <= 1)) {
      message("Some bursts consist of only 1 point, these will be ignored")
      x <- amt::filter_min_n_burst(x, 2)
    }
    l <- lapply(split(x, x$burst_), function(x)
      cbind(x$x_, x$y_))
    l <- sf::st_sf(sf::st_sfc(sf::st_multilinestring(l)))
  } else {
    l <- cbind(x$x_, x$y_)
    l <- sf::st_sf(sf::st_sfc(sf::st_linestring(l)))
  }

  l <- sf::st_set_crs(l,  if (!is.null(attributes(x)$crs_)) attributes(x)$crs_ else sf::NA_crs_)
  l
}

#' @export
as_sf_lines.steps_xy <- function(x, ...) {
  xx <- data.table::setDT(x)
  xx[, linestring_id := .I]
  l <- data.table::melt(xx,
            id.vars = "linestring_id",
            measure.vars = list(x = c("x1_", "x2_"), y = c("y1_", "y2_"))) |>
    data.table::setorder(linestring_id) |>
    sfheaders::sf_line()
  sf::st_set_crs(l,  if (!is.null(attributes(x)$crs_)) attributes(x)$crs_ else sf::NA_crs_)

}

# as_ltraj ----------------------------------------------------------------

#' @export
#' @rdname coercion

as_ltraj <- function(x, ...) {
  UseMethod("as_ltraj", x)
}

#' @export
#' @rdname coercion
as_ltraj.track_xy <- function(x, id = "animal_1", ...) {
  xy <- stats::setNames(coords(x), c("x", "y"))
  if (is.null(list(...)[["id"]])) {
    adehabitatLT::as.ltraj(xy, typeII = FALSE, id = "animal_1", ...)
  } else {
    adehabitatLT::as.ltraj(xy, typeII = FALSE, ...)
  }
}

#' @export
#' @rdname coercion
as_ltraj.track_xyt <- function(x, ...) {
  xy <- stats::setNames(coords(x), c("x", "y"))
  if (is.null(list(...)[["id"]])) {
    adehabitatLT::as.ltraj(xy, date = x$t_, typeII = TRUE, id = "animal_1", ...)
  } else {
    adehabitatLT::as.ltraj(xy, date = x$t_, typeII = TRUE, ...)
  }
}


# as_telemetry -----------------------------------------------------------------
#' @export
#' @rdname coercion
as_telemetry <- function(x, ...) {
  UseMethod("as_telemetry", x)
}

#' @export
#' @rdname coercion
as_telemetry.track_xyt <- function(x, ...) {
  if (!amt::has_crs(x)) {
    stop("track needs to have a crs.")
  }
  x <- transform_coords(x, 4326)

  dat_ctmm <- data.frame(
    lon = x$x_, lat = x$y_, timestamp = x$t_,
    individual.local.identifier = 1
  )

  if ("dop" %in% colnames(x))
    dat_ctmm$dop <- x[["dop"]]
  else if ("DOP" %in% colnames(x))
    dat_ctmm$dop <- x[["DOP"]]
  else if ("hdop" %in% colnames(x))
    dat_ctmm$hdop <- x[["hdop"]]
  else if ("HDOP" %in% colnames(x))
    dat_ctmm$hdop <- x[["HDOP"]]

  suppressMessages(
    ctmm::as.telemetry(
      dat_ctmm
    )
  )
}


# as_moveHMM --------------------------------------------------------------
#' @export
#' @rdname coercion

as_moveHMM <- function(x, ...) {
  UseMethod("as_moveHMM", x)
}

#' @export
#' @rdname coercion
as_moveHMM.track_xy <- function(x, ...) {
  if (grepl("+proj=longlat", attr(x, "crs")$wkt)) {
    moveHMM::prepData(as.data.frame(x), type = "LL", coordNames = c("x_", "y_"))
  } else {
    message("Assuming projected CRS")
    moveHMM::prepData(as.data.frame(x), type = "UTM", coordNames = c("x_", "y_"))
  }
}

