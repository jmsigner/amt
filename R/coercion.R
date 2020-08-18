#' Coerce a track to other formats.
#'
#' Several other packages provides methods to analyze movement data, and `amt` provides coercion methods to some packages
#'
#' @template track_xy_star
#' @param id `[numeric,character,factor]` \cr Animal id(s).
#' @template dots_none
#' @name coercion
#' @export
as_sp <- function(x, ...) {
  UseMethod("as_sp", x)
}

#' @export
as_sp.track_xy <- function(x, ...) {
  sp::SpatialPoints(
    coords = x[, c("x_", "y_")],
    proj4string = if (!is.na(attributes(x)$crs_)) {
      attributes(x)$crs_
    } else {
      sp::CRS(NA_character_)
    }
  )
}

#' @export
#' @param end `[logical(1)=TRUE]` \cr For steps, should the end or start points be used?
#' @rdname coercion
as_sp.steps_xy <- function(x, end = TRUE, ...) {
  if (end) {
    sp::SpatialPoints(
      coords = as.matrix(x[, c("x2_", "y2_")]),
      proj4string = if (!is.null(attributes(x)$crs_)) {
        attributes(x)$crs_
      } else {
        sp::CRS(as.character(NA))
      }
    )
  } else {
    sp::SpatialPoints(
      coords = as.matrix(x[, c("x1_", "y1_")]),
      proj4string = if (!is.null(attributes(x)$crs_)) {
        attributes(x)$crs_
      } else {
        sp::CRS(as.character(NA))
      }
    )
  }
}

# as_sf_points ----------------------------------------------------------------

#' Export track to points
#'
#' Exports a track to points from the `sf` package.
#'
#' @template track_xy_star
#' @template dots_none
#' @return A `tibble` with a `sfc`-column
#' @export

as_sf_points <- function(x, ...) {
  UseMethod("as_sf_points", x)
}

#' @export
as_sf_points.track_xy <- function(x, ...) {

  p <- sf::st_as_sf(x, coords = c("x_", "y_"))
  p <- sf::st_set_crs(p, if (!is.null(attributes(x)$crs_))
    attributes(x)$crs_ else sf::NA_crs_)

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



# as_move() ---------------------------------------------------------------

#' @export
#' @rdname coercion
#' @examples
#' data(deer)
#' as_move(deer)
#' as_move(deer, id = "foo")
as_move <- function(x, ...) {
  UseMethod("as_move", x)
}

#' @export
#' @rdname coercion
as_move.track_xyt <- function(x, id = "id", ...){

  # is a grouping present
  has_id = TRUE

  # Check if id is present
  if (!id %in% names(x)) {
    id <- "unnamed"
    has_id <- FALSE
  }

  # Check for duplicates (group by id)
  any_duplicates <- if (has_id) {
    any(sapply(split(x, x[[id]]), function(y) any(duplicated(y$t_))))
  } else {
    any(duplicated(x$t_))
  }

  if (any_duplicates) {
    warning("data contains duplicates. By default 1st entery is kept, and subsequent duplicates are removed. If this is not wanted, please remove duplicates from original input object")

    x <- if (has_id) {
      do.call(rbind, lapply(split(x, x$t_), function(y)
        y[!duplicated(y$t_), ]))

    } else {
      x[!duplicated(x$t_), ]
    }
  }

  # Create a move object
  move::move(
    x = x$x_,
    y= x$y_,
    time = x$t_,
    data = data.frame(x[!names(x) %in% c("x_","y_","t_")]),
    proj = get_crs(x),
    animal = if (has_id) as.character(x[[id]]) else "unnamed")
}



# as_ltraj ----------------------------------------------------------------

#' @export
#' @rdname coercion
#' @examples
#' data(deer)
#' as_ltraj(deer)
#' as_ltraj(deer, id = "animal_3")
as_ltraj <- function(x, ...) {
  UseMethod("as_ltraj", x)
}

#' @export
#' @rdname coercion
as_ltraj.track_xy <- function(x, id = "animal_1", ...) {
  if (is.null(list(...)[["id"]])) {
    adehabitatLT::as.ltraj(coords(x), typeII = FALSE, id = "animal_1", ...)
  } else {
    adehabitatLT::as.ltraj(coords(x), typeII = FALSE, ...)
  }
}

#' @export
#' @rdname coercion
as_ltraj.track_xyt <- function(x, ...) {
  if (is.null(list(...)[["id"]])) {
    adehabitatLT::as.ltraj(coords(x), date = x$t_, typeII = TRUE, id = "animal_1", ...)
  } else {
    adehabitatLT::as.ltraj(coords(x), date = x$t_, typeII = TRUE, ...)
  }
}


# as_bcpa -----------------------------------------------------------------
#' @export
#' @rdname coercion
#' @examples
#' data(deer)
#' d <- as_bcpa(deer)

as_bcpa <- function(x, ...) {
  UseMethod("as_bcpa", x)
}

#' @export
#' @rdname coercion
as_bcpa.track_xyt <- function(x, ...) {
  x <- tibble(X = x$x_, Y = x$y_, Time = x$t_)
  bcpa::GetVT(x, ...)
}

# as_telemetry -----------------------------------------------------------------
#' @export
#' @rdname coercion
#' @examples
#' data(deer)
#' as_telemetry(deer)
as_telemetry <- function(x, ...) {
  UseMethod("as_telemetry", x)
}

#' @export
#' @rdname coercion
as_telemetry.track_xyt <- function(x, ...) {
  if (!amt::has_crs(x)) {
    stop("track needs to have a CRS.")
  }
  x <- transform_coords(x, sp::CRS("+init=epsg:4326"))
  ctmm::as.telemetry(data.frame(lon = x$x_, lat = x$y_, timestamp = x$t_))

}


# as_moveHMM --------------------------------------------------------------
#' @export
#' @rdname coercion
#' @examples
#' # Fit HMM with two states
#' data(deer)
#' dm <- as_moveHMM(deer)

as_moveHMM <- function(x, ...) {
  UseMethod("as_moveHMM", x)
}

#' @export
#' @rdname coercion
as_moveHMM.track_xy <- function(x, ...) {
  if (grepl("+proj=longlat", attr(x, "crs"))) {
    moveHMM::prepData(as.data.frame(x), type = "LL", coordNames = c("x_", "y_"))
  } else {
    message("Assuming projected CRS")
    moveHMM::prepData(as.data.frame(x), type = "UTM", coordNames = c("x_", "y_"))
  }
}



