#' Coerce a track to other formats.
#'
#' Several other packages provides methods to analyse movement data, and `amt` provides coercion methods to some packages
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
    proj4string = if (!is.null(attributes(x)$crs_)) {
      attributes(x)$crs_
    } else {
      sp::CRS(as.character(NA))
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
  if (is.null(get_crs(x))) {
    stop("move requires a projected track.")
  }
  move::move(x = x$x_, y = x$y_, proj = get_crs(x), ...)
}

#' @export
#' @rdname coercion
as_move.track_xyt <- function(x, ...) {
  if (is.null(get_crs(x))) {
    stop("move requires a projected track.")
  }
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
#' \dontrun{
#' bcpa1 <- bcpa::WindowSweep(d, "Theta", K = 2, windowsize = 50)
#' plot(bcpa1, type = "flat", clusterwidth = 1)
#' }

as_bcpa <- function(x, ...) {
  UseMethod("as_bcpa", x)
}

#' @export
#' @rdname coercion
as_bcpa.track_xyt <- function(x, ...) {
  x <- data_frame(X = x$x_, Y = x$y_, Time = x$t_)
  bcpa::GetVT(x, ...)
}

# as_telemetry -----------------------------------------------------------------
#' @export
#' @rdname coercion
#' @examples
#' data(deer)
#' as_ctmm(deer)
as_telemetry <- function(x, ...) {
  UseMethod("as_telemetry", x)
}

#' @export
#' @rdname coercion
as_telemetry.track_xyt <- function(x, ...) {
  if (!amt::has_crs(x)) {
    stop("track needs to havea CRS.")
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
#' \dontrun{
#' mu0 <- rep(mean(dm$step, na.rm = TRUE), 2) # step mean (two parameters: one for each state)
#' sigma0 <- rep(sd(dm$step, na.rm = TRUE), 2) # step SD
#' zeromass0 <- c(0.1, 0.05) # step zero-mass
#' stepPar0 <- c(mu0, sigma0, zeromass0)
#' angleMean0 <- c(pi, pi) # angle mean
#' kappa0 <- c(1, 1) # angle concentration
#' anglePar0 <- c(angleMean0, kappa0) ## call to fitting function
#' m1 <- fitHMM(data = dm, nbStates = 2,
#'        stepPar0 = stepPar0, anglePar0 = anglePar0, formula = ~ 1)
#' }
#'
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



