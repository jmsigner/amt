#' Directions
#'
#' Functions to calculate the relative and absolute direction of an animal
#' @export
#' @param x A track.
#' @param degrees Logcial scalar, if `TRUE` angles are returned in degrees, otherwise in radians.
#' @param full_circle Logcial scalar, if `TRUE` angles are returned between 0 and 360 degrees or 0 and 2 pi (depnding on the value of `degrees`), otherwise angles are between -180 and 180 or -pi and pi.
#' @param zero_dir Character scalar, must be either 'N', 'E', 'S' or 'W' and indicates the zero direction.
#' @param clockwise Logical scalar, should angles be calculated clock or anti-clockwise.
#' @param append_last Logical scalar, if `TRUE` an `NA` is appended at the end of all angles.
#' @name direction
#'
direction_abs <- function(x, ...) {
  UseMethod("direction_abs", x)
}

#' @export
#' @rdname direction
#' @examples
#' # Absolute directions
#'
#' x <- c(1, 4, 8, 8, 12, 8, 0, 0, 4, 2)
#' y <- c(0, 0, 0, 8, 12, 12, 12, 8, 4, 2)
#' trk <- track(x = x, y = y)
#'
#' # append last
#' direction_abs(trk, append_last = TRUE)
#' direction_abs(trk, append_last = FALSE)
#'
#' # degrees
#' direction_abs(trk, degrees = FALSE)
#' direction_abs(trk, degrees = TRUE)
#'
#' # full circle or not
#' direction_abs(trk, degrees = TRUE, full_circle = TRUE)
#' direction_abs(trk, degrees = TRUE, full_circle = FALSE)
#'
#' # direction of 0
#' direction_abs(trk, full_circle = TRUE, zero_dir = "N")
#' direction_abs(trk, full_circle = TRUE, zero_dir = "E")
#' direction_abs(trk, full_circle = TRUE, zero_dir = "S")
#' direction_abs(trk, full_circle = TRUE, zero_dir = "W")
#'
#' # clockwise or not
#' direction_abs(trk, full_circle = TRUE, zero_dir = "N", clockwise = FALSE)
#' direction_abs(trk, full_circle = TRUE, zero_dir = "N", clockwise = TRUE)
#'
#' # Bearing (i.e. azimuth): only for lon/lat
#' direction_abs(trk, full_circle = FALSE, zero_dir = "N", planar = TRUE, clockwise = TRUE)
#' direction_abs(trk, full_circle = FALSE, zero_dir = "N", planar = FALSE, clockwise = TRUE)
#'
#' # How do results compare to other packages
#'
#' # adehabitatLT
#' df <- adehabitatLT::as.ltraj(data.frame(x = x, y = y), typeII = FALSE, id = 1)
#' df[[1]]$abs.angle
#' amt::direction_abs(trk, degrees = FALSE, full_circle = FALSE)
#'
#' # bcpa
#' df <- bcpa::MakeTrack(x, y, now() +  hours(1:10))
#' bcpa::GetVT(df)$Phi
#' amt::direction_abs(trk, degrees = FALSE, full_circle = FALSE, append_last = FALSE)
#'
#' # move
#' m <- move::move(x, y, now() + hours(1:10), proj = CRS("+init=epsg:4326"))
#' move::angle(m)
#' direction_abs(trk, degrees = TRUE, full_circle = FALSE, zero_dir = "N",
#'   clockwise = TRUE, append_na = FALSE, planar = FALSE)
#'
#' # trajectories
#' library(trajectories)
#' library(spacetime)
#' t1 <- Track(STIDF(SpatialPoints(cbind(x, y)), now() + hours(1:10), data = data.frame(1:10)))
#'
#' t1[["direction"]]
#' direction_abs(trk, degrees = TRUE, full_circle = TRUE, zero_dir = "N", clockwise = TRUE, append_last = FALSE)
#'
#' # moveHMM (only rel. ta)
#' df <- data.frame(ID = 1, x = x, y = y)
#' moveHMM::prepData(df, type = "UTM")





direction_abs.track_xy <- function(x, degrees = TRUE, full_circle = FALSE, zero_dir = "E", clockwise = FALSE,
                                   append_last = TRUE, planar = TRUE, ...) {
  zero_dir <- toupper(zero_dir)
  if (!zero_dir %in% c("E", "N", "W", "S")) {
    stop("zero_dir should be in either 'E', 'N', 'W', or 'S'")
  }

  # angles
  a <- if (planar) {
    atan2(diff_y(x), diff_x(x)) * 180 / pi
  } else {
    xx <- sp::coordinates(as_sp(x))
    (450 + ((360 - geosphere::bearing(xx[-nrow(xx), ], xx[-1, ]))) %% 360) %% 360
  }


  # remove last NA
  a <- if (!append_last) a[-length(a)] else a
  a <- ifelse(a < 0, 360 + a, a)
  a <- switch(zero_dir,
         E = a,
         S = (450 + a) %% 360,
         W = (540 + a) %% 360,
         N = (630 + a) %% 360
  )
  a <- if (clockwise)  (360 - a) %% 360 else a
  a <- if (full_circle) a else ifelse(a > 180, (360 - a) * -1, a)
  a * if (degrees) 1 else pi / 180
}

# Directions rel ----------------------------------------------------------
#' @rdname direction
#' @export
direction_rel <- function(x, ...) {
  UseMethod("directoin_rel", x)
}

#' @export
#' @rdname direction
direction_rel <- function(x, degrees = TRUE) {
  p <- c(NA, diff_rcpp(direction_abs(x, degrees = FALSE)))
  p <- ifelse( p <= (-pi), p + 2 * pi, p)
  p <- ifelse( p > pi, p - 2 * pi, p)
  p * if (degrees) 180 / pi else 1
}


