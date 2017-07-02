#' Track coercion functions
#'
#' amt provides several coercion functions that operate on `tracks` and `steps`.
#'
#' The following coercion functions are currently provided:
#'  * `as_sp` turns a track into `SpatialPoints`.
#' @param x A `track_xy`.
#' @template dots_none
#' @details
#' @name coercion
#' @export
as_sp <- function(x, ...) {
  UseMethod("as_sp", x)
}

#' @export
#' @rdname coercion
as_sp.track_xy <- function(x, ...) {
  sp::SpatialPoints(
    coords = x[, c("x_", "y_")],
    proj4string = if (!is.null(attributes(x)$crs_)) {
      attributes(x)$crs_
    } else {
      sp::CRS(as.character(NA))
    }
  )
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
as_move.track_xy <- function(x, ...) {
  move::move(x = x$x_, y = x$y_, proj = get_crs(x), ...)
}

#' @export
#' @rdname coercion
as_move.track_xyt <- function(x, ...) {
  move::move(x = x$x_, y = x$y_, time = x$t_, proj = get_crs(x), ...)
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
#' @examples
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
#' as_bcpa(deer)
as_bcpa <- function(x, ...) {
  UseMethod("as_bcpa", x)
}

#' @export
#' @rdname coercion
#' @examples
as_bcpa.track_xyt <- function(x, id = "animal_1", ...) {
  message("not yet implemented")
}




# as_ctmm -----------------------------------------------------------------
#' @export
#' @rdname coercion
#' @examples
#' data(deer)
#' as_ctmm(deer)
as_ctmm <- function(x, ...) {
  UseMethod("as_ctmm", x)
}

#' @export
#' @rdname coercion
#' @examples
as_ctmm.track_xyt <- function(x, id = "animal_1", ...) {
  message("not yet implemented")
}


# as_moveHMM --------------------------------------------------------------
#' @export
#' @rdname coercion
#' @examples
#' data(deer)
#' as_moveHMM(deer)
as_moveHMM <- function(x, ...) {
  UseMethod("as_moveHMM", x)
}

#' @export
#' @rdname coercion
#' @examples
as_moveHMM.track_xy <- function(x, ...) {
  if (grepl("+proj=longlat", attr(x, "crs"))) {
    moveHMM::prepData(as.data.frame(x), type = "LL", coordNames = c("x_", "y_"))
  } else {
    message("Assuming projected CRS")
    moveHMM::prepData(as.data.frame(x), type = "UTM", coordNames = c("x_", "y_"))
  }
}

