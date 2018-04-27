#' Functions to create and work with steps
#'
#' `step_lengths` can be use to calculate step lengths of a track. `direction_abs` and `direction_rel` calculate the absolute and relative direction of steps. `steps` converts a `track_xy*` from a point representation to a step representation and automatically calculates step lengths and relative turning angles.
#'
#' @template track_xy_star
#' @param lonlat `[logical(1)=TRUE]` \cr Should geographical or planar coordinates be used? If `TRUE` geographic distances are calculated.
#' @param degrees `[logical(1)=TRUE]` \cr Should turn angles be calculated in degrees or radians? If `TRUE` angles are returned in degrees, otherwise in radians.
#' @param full_circle `[logical(1)=FALSE]` \cr If `TRUE` angles are returned between 0 and 360 degrees or 0 and $2pi$ (depending on the value of `degrees`), otherwise angles are between -180 and 180 or $-pi$ and $pi$.
#' @param zero_dir `[character(1)='E']` \cr Indicating the zero direction. Must be either `N`, `E`, `S`, or `W`.
#' @param clockwise `[logical(1)=FALSE]` \cr Should angles be calculated clock or anti-clockwise?
#' @param append_last `[logical(1)=TRUE]` \cr If `TRUE` an `NA` is appended at the end of all angles.
#' @param keep_cols `[character(1)=NULL]{'start', 'end'}` \cr Should columns with attribute information be transfered to steps? If `keep_cols = 'start'` the attributes from the starting point are use, otherwise the columns from the end points are used.
#' @param ... Further arguments, none implemented
#'
#' @return `[numeric]` \cr For `step_lengths()` and `direction_*` a numeric vector. \cr
#' `[data.frame]` \cr For `steps` and `steps_by_burst`, containing the steps.
#' @name steps
NULL



# directions --------------------------------------------------------------

#' @export
#' @rdname steps
#'
direction_abs <- function(x, ...) {
  UseMethod("direction_abs", x)
}

#' @export
#' @rdname steps
#' @examples
#'
# directions --------------------------------------------------------------
#'
#' # Absolute directions
#'
#' xy <- data_frame(
#'   x = c(1, 4, 8, 8, 12, 8, 0, 0, 4, 2),
#'   y = c(0, 0, 0, 8, 12, 12, 12, 8, 4, 2))
#' trk <- mk_track(xy, x, y)
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
#' direction_abs(trk, full_circle = FALSE, zero_dir = "N", lonlat = FALSE, clockwise = TRUE)
#' direction_abs(trk, full_circle = FALSE, zero_dir = "N", lonlat = TRUE, clockwise = TRUE)
#'
#' # How do results compare to other packages
#' # adehabitatLT
#' df <- adehabitatLT::as.ltraj(data.frame(x = xy$x, y = xy$y), typeII = FALSE, id = 1)
#' df[[1]]$abs.angle
#' amt::direction_abs(trk, degrees = FALSE, full_circle = FALSE)
#'
#' # bcpa
#' df <- bcpa::MakeTrack(xy$x, xy$y, lubridate::now() +  lubridate::hours(1:10))
#' bcpa::GetVT(df)$Phi
#' direction_abs(trk, degrees = FALSE, full_circle = FALSE, append_last = FALSE)
#'
#' # move
#' m <- move::move(xy$x, xy$y, lubridate::now() + lubridate::hours(1:10),
#'  proj = sp::CRS("+init=epsg:4326"))
#' move::angle(m)
#' direction_abs(trk, degrees = TRUE, full_circle = FALSE, zero_dir = "N",
#'   clockwise = TRUE, append_na = FALSE, lonlat = TRUE)
#'
#' # trajectories
#' t1 <- trajectories::Track(
#'   spacetime::STIDF(sp::SpatialPoints(cbind(xy$x, xy$y)),
#'   lubridate::now(tzone = "UTC") + lubridate::hours(1:10), data = data.frame(1:10)))
#'
#' t1[["direction"]]
#' direction_abs(trk, degrees = TRUE, full_circle = TRUE, zero_dir = "N",
#'   clockwise = TRUE, append_last = FALSE)
#'
#' # moveHMM (only rel. ta)
#' df <- data.frame(ID = 1, x = xy$x, y = xy$y)
#' moveHMM::prepData(df, type = "UTM")


direction_abs.track_xy <- function(x, degrees = TRUE, full_circle = FALSE, zero_dir = "E",
                                   clockwise = FALSE,
                                   append_last = TRUE, lonlat = FALSE, ...) {
  zero_dir <- toupper(zero_dir)
  if (!zero_dir %in% c("E", "N", "W", "S")) {
    stop("zero_dir should be in either 'E', 'N', 'W', or 'S'")
  }

  if (zero_dir == "E") {
    zero_dir <- "East"
  }

  # angles
  a <- if (!lonlat) {
    atan2(diff_y(x), diff_x(x)) * 180 / pi
  } else {
    xx <- sp::coordinates(as_sp(x))
    c((450 + ((360 - geosphere::bearing(xx[-nrow(xx), ], xx[-1, ]))) %% 360) %% 360, NA)
  }


  # remove last NA
  a <- if (append_last) a else a[-length(a)]
  a <- ifelse(a < 0, 360 + a, a)
  a <- switch(zero_dir,
         "East" = a,  # to avoid partial matching with EXPR
         "S" = (450 + a) %% 360,
         "W" = (540 + a) %% 360,
         "N" = (630 + a) %% 360
  )
  a <- if (clockwise)  (360 - a) %% 360 else a
  a <- if (full_circle) a else ifelse(a > 180, (360 - a) * -1, a)
  a * if (degrees) 1 else pi / 180
}

# Directions rel ----------------------------------------------------------
#' @rdname steps
#' @export
#' @examples
#' # How do results compare to other packages
#' xy <- data_frame(
#'   x = c(1, 4, 8, 8, 12, 8, 0, 0, 4, 2),
#'   y = c(0, 0, 0, 8, 12, 12, 12, 8, 4, 2))
#' trk <- mk_track(xy, x, y)
#' # adehabitatLT
#' df <- adehabitatLT::as.ltraj(data.frame(x = xy$x, y = xy$y), typeII = FALSE, id = 1)
#' df[[1]]$rel.angle
#' amt::direction_rel(trk, degrees = FALSE, full_circle = FALSE)
#'
# # bcpa
# df <- bcpa::MakeTrack(xy$x, xy$y, lubridate::now() +  lubridate::hours(1:10))
# bcpa::GetVT(df)$Theta
# direction_rel(trk, degrees = FALSE, append_last = FALSE)
#
# # move
# m <- move::move(xy$x, xy$y, lubridate::now() + lubridate::hours(1:10),
# proj = sp::CRS("+init=epsg:4326"))
# move::turnAngleGc(m)
# direction_abs(trk, degrees = TRUE, full_circle = FALSE, zero_dir = "N",
#   clockwise = TRUE, append_na = FALSE, lonlat = TRUE)
#
#' # trajectories
#' t1 <- trajectories::Track(
#'   spacetime::STIDF(sp::SpatialPoints(cbind(xy$x, xy$y)),
#'   lubridate::now() + lubridate::hours(1:10), data = data.frame(1:10)))
#'
#' t1[["direction"]]
#' direction_abs(trk, degrees = TRUE, full_circle = TRUE, zero_dir = "N",
#'   clockwise = TRUE, append_last = FALSE)
#'
#' # moveHMM (only rel. ta)
#' df <- data.frame(ID = 1, x = xy$x, y = xy$y)
#' moveHMM::prepData(df, type = "UTM")
direction_rel <- function(x, ...) {
  UseMethod("direction_rel", x)
}

#' @export
#' @rdname steps
#' @examples
#'
# direction_rel -----------------------------------------------------------
#' trk
#'
direction_rel.track_xy <- function(x, lonlat = FALSE, degrees = TRUE, append_last = TRUE,
                                   zero_dir = "E", ...) {

  p <- direction_abs(x, degrees = FALSE, lonlat = lonlat, full_circle = FALSE,
                     zero_dir = zero_dir, clockwise = FALSE, append_last = append_last)
  p <- c(NA, diff_rcpp(p)) %% (2 * pi)
  #p <- ifelse(p <= (-pi), p + 2 * pi, p)
  p <- ifelse( p > pi, p - 2 * pi, p)
  p * if (degrees) 180 / pi else 1
}




# steps -------------------------------------------------------------------

# step lengths ------------------------------------------------------------

#' @export
#' @rdname steps
#' @details `step_lengths` calculates the step lengths between points a long the path. The last value returned is `NA`, because no observed step is 'started' at the last point. If `lonlat = TRUE`, `step_lengths()` wraps [raster::pointDistance()].
#' @examples
#' # step_lengths ------------------------------------------------------------
#' xy <- data_frame(
#'   x = c(0, 1, 2),
#'   y = c(0, 1, 2)
#' )
#' xy <- mk_track(xy, x, y)
#'
#' step_lengths(xy, lonlat = FALSE)
#' step_lengths(xy, lonlat = TRUE) # in m, but coords are assumed in degrees
#'
#'
#' # creating steps ----------------------------------------------------------
#'
#' # Create some dummy data
#' library(lubridate)
#' df <- data_frame(
#'   x = runif(10),
#'   y = runif(10),
#'   a = runif(10),
#'   t = now() + hours(c(1:2, 5:6, 9:10, 14:17)),
#'   b = 3,
#'   c = a + 30
#' )
#'
#' library(amt)
#' make_track(df, x, y, t, all_cols = TRUE) %>%
#'   steps(keep_cols = "start")
#'
#' make_track(df, x, y, all_cols = TRUE) %>%
#'   steps(keep_cols = "end")
#'
#' make_track(df, x, y, t, all_cols = TRUE) %>%
#'   track_resample(rate = hours(1), tolerance = minutes(5)) %>%
#'   steps_by_burst(keep_cols = "start")
#'
#' make_track(df, x, y, t, all_cols = TRUE) %>%
#'   track_resample(rate = hours(1), tolerance = minutes(5)) %>%
#'   steps_by_burst(keep_cols = NULL)
#'




step_lengths <- function(x, ...) {
  UseMethod("step_lengths", x)
}

#' @export
#' @rdname steps
step_lengths.track_xy <- function(x, lonlat = FALSE, append_last = TRUE, ...) {
  if (lonlat) {
    pts <- sp::coordinates(as_sp(x))
    q <- c(raster::pointDistance(pts[-nrow(pts), ], pts[-1, ], lonlat = TRUE), NA)
  } else {
    q <- sqrt(step_lengths_sq(x))
  }
  if (append_last) q else q[-length(q)]
}

#' @noRd
step_lengths_sq <- function(x, ...) {
  UseMethod("step_lengths_sq", x)
}

#' @noRd
step_lengths_sq.track_xy <- function(x, ...) {
  diff_x(x)^2 + diff_y(x)^2
}

#' @noRd
distance_with_diff <- function(xd, yd) {
  c(NA, sqrt((xd)^2 + (yd)^2))
}


# steps -------------------------------------------------------------------

#' @export
#' @rdname steps
steps_by_burst <- function(x, ...) {
  UseMethod("steps_by_burst", x)
}

#' @rdname steps
#' @export
steps_by_burst.track_xyt <- function(x, lonlat = FALSE,
                                     degrees = TRUE,
                                     keep_cols = NULL, ...) {

  togo <- cumsum(rle(x$burst_)$lengths)
  ss <- suppressWarnings(steps(x, lonlat = lonlat, keep_cols = keep_cols, ...))

  if (!"burst_" %in% names(ss)) {
    ss <- tibble::add_column(ss, burst_ = x$burst_[-1], .before = 1)
  }

  if (!degrees) {
    ss$ta_ <- ss$ta_ * pi / 180
  }

  ss[head(togo, -1) + 1, "ta_"] <- NA
  ss <- ss[-togo, ]
  class(ss) <- c("steps", class(x)[-(1:2)])
  attr(ss, "crs_") <- attr(x, "crs_")
  ss
}


#' @export
#' @rdname steps
steps <- function(x, ...) {
  UseMethod("steps", x)
}

#' @export
#' @rdname steps
steps.track_xy <- function(x, lonlat = FALSE,
                           keep_cols = NULL,
                           degrees = TRUE, ...) {
  n <- nrow(x)
  xx <- steps_base(x, n, lonlat = lonlat, keep_cols = keep_cols)

  if (!degrees) {
    xx$ta_ <- xx$ta_ * pi / 180
  }

  class(xx) <- c("steps", class(x)[-1])
  attr(xx, "crs_") <- attr(x, "crs_")
  xx
}

#' @export
#' @param diff_time_units `[character(1)='auto']` \cr The unit for time differences, see `?difftime`.
#' @rdname steps
steps.track_xyt <- function(x, lonlat = FALSE, degrees = TRUE,
                            keep_cols = NULL,
                            diff_time_units = "auto", ...) {
  n <- nrow(x)
  if ("burst_" %in% names(x)) {
    warning("burst's are ignored, use steps_by_burst instead.")
  }
  xx <- steps_base(x, n, lonlat, keep_cols = keep_cols)
  xx$t1_ <- x$t_[-n]
  xx$t2_ <- x$t_[-1]
  xx$dt_ <- difftime(xx$t2_,  xx$t1_, units = diff_time_units)

  if (!degrees) {
    xx$ta_ <- xx$ta_ * pi / 180
  }

  class(xx) <- c("steps", class(x)[-(1:2)])
  attr(xx, "crs_") <- attr(x, "crs_")
  xx
}


steps_base <- function(x, n, lonlat, degrees, zero_dir, keep_cols) {
  out <- data_frame(
    x1_ = x$x_[-n],
    x2_ = x$x_[-1],
    y1_ = x$y_[-n],
    y2_ = x$y_[-1],
    sl_ = step_lengths(x, lonlat = lonlat, append_last = FALSE),
    ta_ = direction_rel(x, lonlat = lonlat, degrees = TRUE, zero_dir = "E", append_last = FALSE)
  )

  if (!is.null(keep_cols)) {

    if (keep_cols == "start") {
      out <- dplyr::bind_cols(
        out,
        x[-n, base::setdiff(names(x), c("x_", "y_", if (is(x, "track_xyt")) "t_"))])
    } else  {
      out <- dplyr::bind_cols(
        out,
        x[-1, base::setdiff(names(x), c("x_", "y_", if (is(x, "track_xyt")) "t_"))])
    }
  }
  out
}

steps_transfer_attr <- function(from, to) {
  from <- attributes(from)
  attributes(to)$class <- from$class
  attributes(to)$sl_ <- from$sl_
  attributes(to)$ta_ <- from$ta_
  attributes(to)$crs_ <- from$crs_
  to
}

#' @export
`[.steps` <- function(x, i, j, drop = FALSE) {
  xx <- NextMethod()
  steps_transfer_attr(x, xx)
}

# see here: https://github.com/hadley/dplyr/issues/719
#' @export
arrange.steps <- function(.data, ..., .dots) {
  xx <- NextMethod()
  steps_transfer_attr(.data, xx)
}

#' @export
filter.steps <- function(.data, ..., .dots) {
  xx <- NextMethod()
  steps_transfer_attr(.data, xx)
}

#' @export
group_by.steps <- function(.data, ..., .dots) {
  xx <- NextMethod()
  steps_transfer_attr(.data, xx)
}

#' @export
mutate.steps <- function(.data, ..., .dots) {
  xx <- NextMethod()
  steps_transfer_attr(.data, xx)
}

#' @export
select.steps <- function(.data, ..., .dots) {
  xx <- NextMethod()
  steps_transfer_attr(.data, xx)
}

#' @export
summarise.steps <- function(.data, ..., .dots) {
  xx <- NextMethod()
  steps_transfer_attr(.data, xx)
}


#' @export
summarize.steps <- function(.data, ..., .dots) {
  xx <- NextMethod()
  steps_transfer_attr(.data, xx)
}

