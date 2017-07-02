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
#' d <- as_bcpa(deer)
#' bcpa1 <- WindowSweep(d, "Theta", K = 2, windowsize = 50)
#' plot(bcpa1, type = "flat", clusterwidth = 1)

as_bcpa <- function(x, ...) {
  UseMethod("as_bcpa", x)
}

#' @export
#' @rdname coercion
#' @examples
as_bcpa.track_xyt <- function(x, ...) {
  x <- dplyr::rename(x, X = x_, Y = y_, Time = t_)
  bcpa::GetVT(x, ...)
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
#' # Fit HMM with two states
#' data(deer)
#' dm <- as_moveHMM(deer)
#' mu0 <- rep(mean(dm$step, na.rm = TRUE), 2) # step mean (two parameters: one for each state)
#' sigma0 <- rep(sd(dm$step, na.rm = TRUE), 2) # step SD
#' zeromass0 <- c(0.1, 0.05) # step zero-mass
#' stepPar0 <- c(mu0, sigma0, zeromass0)
#' angleMean0 <- c(pi, pi) # angle mean
#' kappa0 <- c(1, 1) # angle concentration
#' anglePar0 <- c(angleMean0, kappa0) ## call to fitting function
#' m1 <- fitHMM(data = dm, nbStates = 2,
#'        stepPar0 = stepPar0, anglePar0 = anglePar0, formula = ~ 1)
#'
#'
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

