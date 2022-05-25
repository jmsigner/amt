# Telemetry data cleaning functions
# Based on code written by Tal Avgar in MATLAB
# Originally translated to R by Johannes Signer
# Updates by Brian J. Smith in May 2022

# Helpers ----

#' Extract sampling period
#'
#' Extracts sampling period from a `track_xyt` object
#'
#' @param x `[track_xyt]` A `track_xyt` object.
#' @template dots_none
#'
#' @export
sampling_period <- function(x, ...) {
  range(x$t_)
}

#' Summarize step lengths
#'
#' Summarizes step lengths for a `track_xy*` object
#'
#' @param x `[track_xy, track_xyt]` A `track_xy*` object.
#' @template dots_none
#'
#' @export
summarize_sl <- function(x, ...) {
  summary(step_lengths(x, append_last = FALSE))
}

#' Summarize speed
#'
#' Summarizes speeds for a `track_xyt` object
#'
#' @param x `[track_xyt]` A `track_xyt` object.
#' @template dots_none
#'
#' @export
summarize_speed <- function(x, ...) {
  summary(speed(x))
}

#' Subset to tracking dates
#'
#' Subsets a `track_xyt` object
#'
#' @param x `[track_xy, track_xyt]` A `track_xy*` object.
#' @param from `[POSIXt]` A date and time defining start of subset.
#' @param to `[POSIXt]` A date and time defining end of subset.
#'
#' @export
tracked_from_to <- function(x, from = min(x$t_), to = max(x$t_)) {
  x[x$t_ >= from & x$t_ <= to, ]
}


# SDR ----

#' Calculate SDR
#'
#' Calculates squared displacement rate for a given speed and duration
#'
#' @param speed `[numeric]` A speed given in either km/h or m/s.
#' @param time `[Period]` A `lubridate` `Period` for which the `speed`
#' can be sustained.
#' @param speed_unit `[character]` The unit in which `speed` is given. Should
#' be either `"km/h"` or `"m/s"`.
#'
#' @seealso \code{\link{get_displacement}()}
#'
#' @return Returns a numeric vector (of length 1) with the SDR in `m^2/s`.
#'
#' @author Johannes Signer and Brian J. Smith
#'
#' @examples
#'
#' # Assume a cheetah can sprint 100 km/h for 60 seconds
#' calculate_sdr(speed = 100, time = seconds(60), speed_unit = "km/h")
#' # 46296.3 m^2/s
#'
#' # What is expected displacement in 1 h at that SDR?
#' get_displacement(46296.3, hours(1))
#' # 12909.95 m = 12.9 km/h (much slower than sprint speed!)
#'
#' @export
calculate_sdr <- function(speed = 50, time, speed_unit = c("km/h", "m/s")) {
  checkmate::assert_numeric(speed, lower = 0, len = 1)
  checkmate::assert_class(time, "Period")
  checkmate::assert_character(speed_unit)
  if (length(speed_unit) > 1) {
    speed_unit <- speed_unit[1]
    message(paste0("Assuming speed is provided in ", speed_unit, "."))
  }
  if (!speed_unit %in% c("km/h", "m/s")) {
    stop("`speed` needs to have unit km/h or m/s.")
  }
  # transform speed in m/s
  speed_ms <- if (speed_unit == "km/h") {
    speed * 1e3 / 3600
  } else {
    speed
  }
  delta <- (speed_ms^2 * as.numeric(time))
  # return
  return(delta)
}

#' Calculate Expected Displacement
#'
#' Calculates expected displacement for a given SDR and time span
#'
#' @param delta `[numeric]` A squared displacement rate (SDR), such as that
#' returned by \code{\link{calculate_sdr}()}. Units assumed to be `m^2/s`.
#' @param time_span `[Period]` A `lubridate` `Period` giving the time span
#' for which to calculate expected displacement.
#'
#' @seealso \code{\link{calculate_sdr}()}
#'
#' @return Returns a numeric vector (of length 1) with the expected displacement
#' in meters.
#'
#' @author Johannes Signer and Brian J. Smith
#'
#' @export
get_displacement <- function(delta, time_span) {
  checkmate::assert_numeric(time_span, lower = 0, finite = TRUE)
  checkmate::assert_class(time_span, "Period")
  return(sqrt(delta * as.numeric(time_span, units = "secs")))
}

# BJS comment: adapted this function to be similar to existing 'amt' functions
#' Calculate SDR for an Object
#'
#' Calculates SDR for an object of certain class
#'
#' @param x `[track_xyt]` Object to calculate SDR from. Currently implemented
#' for `track_xyt`.
#' @param time_unit `[character]` Character string giving time unit. Should be
#' `"secs"`, `"mins"`, or `"hours"`.
#' @param append_na `[logical]` Should `NA` be appended to the end of the vector?
#' Ensures `length(result) == nrow(x)` if appending as a column of `x`.
#' @template dots_none
#'
#' @author Brian J. Smith and Johannes Signer
#'
#' @details `time_unit` defaults to seconds because \code{\link{calculate_sdr}()}
#' returns SDR in m^2/s. We assume the user is also working in a projected
#' CRS with units in meters, thus we expect SDR in m^2/s to be most relevant.
#'
#' @seealso \code{\link{calculate_sdr}()}, \code{\link{get_displacement}()}
#'
#' @name sdr
#' @export
sdr <- function(x, time_unit = "secs", append_na = TRUE, ...) {
  UseMethod("sdr", x)
}


#' @rdname sdr
#' @export
sdr.track_xyt <- function(x, time_unit = "secs", append_na = TRUE, ...) {
  ## Input checks
  # time_unit
  check_time_unit(time_unit)

  stps <- suppressWarnings(steps(x))
  s2 <- stps$sl_^2 / as.numeric(stps$dt_, units = time_unit)
  if (append_na) {
    return(c(s2, NA))
  } else {
    return(s2)
  }
}

# Speed and dNSD ----
#' Calculate Speed
#'
#' Calculates speed
#'
# BJS comment: not sure why this wrapper around amt::speed(). Probably
# not worth exporting this function, but might be a good idea to do
# the infinite speed check in amt::speed().
# calculate_speed <- function(x) {
#   v <- speed(x)
#   if (any(!is.finite(v))) {
#     warning("Speeds contain infinite values, are there duplicated timestamps?")
#   }
#   v[!is.finite(v)] <- NA
#   c(v, NA)
# }

#' Calculate Change in NSD
#'
#' Calculates change in NSD
#'
# BJS comment: not sure if we're using this for anything currently.
# Not currently exporting. Might add it back in (with documentation).
# calcualte_dnsd <- function(x) {
#   abs(diff(nsd(x))) / as.numeric(diff(x$t_, unit = "sec"))
# }

# Internal checks ----

#' Check `time_unit` Parameter
#'
#' Internal function to check `time_unit` parameter in various cleaning functions.
#' @param tu The `time_unit` parameter to check.
#'
check_time_unit <- function(tu) {
  # Time unit
  if (!(tu %in% c("secs", "mins", "hours"))) {
    stop('"time_unit" should be one of "secs", "mins", "hours".')
  }
}

# Duplicates ----
#' Flag Low Quality Duplicates
#'
#' Flags locations with duplicate timestamps by DOP and distance
#'
#' @param x `[track_xyt]` A `track_xyt` object.
#' @param gamma `[numeric or Period]` The temporal tolerance defining duplicates.
#' See details below. If `numeric`, its units are defined by `time_unit`. If
#' `Period`, `time_unit` is ignored.
#' @param time_unit `[character]` Character string giving time unit for `gamma`.
#' Should be `"secs"`, `"mins"`, or `"hours"`. Ignored if `class(gamma) == "Period".`
#' @param DOP `[character]` A character string giving the name of the column
#' containing the dilution of precision (DOP) data. See details below.
#' @param ... Addtional arguments. None currently implemented.
#'
#' @details Locations are considered duplicates if their timestamps are within
#' `gamma` of each other. However, the function runs sequentially through the
#' track object, so that only timestamps after the focal point are flagged as
#' duplicates (and thus removed from further consideration). E.g., if
#' `gamma = minutes(5)`, then all locations with timestamp within 5 minutes
#' after the focal location will be considered duplicates.
#'
#' When duplicates are found, (1) the location with the lowest dilution of precision
#' (given by `DOP` column) is kept. If there are multiple duplicates with equally
#' low DOP, then (2) the one closest in space to previous location is kept. In
#' the event of exact ties in DOP and distance, (3) the first location is kept.
#' This is unlikely unless there are exact coordinate duplicates.
#'
#' In the case that the first location in a trajectory has a duplicate, there
#' is no previous location with which to calculate a distance. In that case,
#' the algorithm skips to (3) and keeps the first location.
#'
#' In the event your `data.frame` does not have a DOP column, you can insert a dummy
#' with constant values such that all duplicates will tie, and distance will be
#' the only criterion (e.g., `x$dop <- 1`). In the event you do have an alternate
#' measure of precision where larger numbers are more precise (e.g., number of
#' satellites), simply multiply that metric by `-1` and pass it as if it were DOP.
#'
#' Internally, the function drops duplicates as it works sequentially through the
#' `data.frame`. E.g., if location 5 was considered a duplicate of location 4 --
#' and location 4 was higher quality -- then location 5 would be dropped. The
#' function would then move on to location 6 (since 5 was already dropped).
#' However, the object returned to the user has all the original rows of `x` --
#' i.e., locations are flagged rather than removed.
#'
#' @return Returns `x` (a `track_xyt`) with a flagging column added (`x$duplicate_`).
#'
#' @seealso \code{\link{flag_fast_steps}()},
#' \code{\link{flag_roundtrips}()},
#' \code{\link{flag_defunct_clusters}()}
#'
#' @author Brian J. Smith, based on code by Johannes Signer and Tal Avgar
#'
#' @name flag_duplicates
#' @export
flag_duplicates <- function(x, gamma, time_unit = "mins",
                            DOP = "dop", ...) {
  UseMethod("flag_duplicates", x)
}

#' @rdname flag_duplicates
#' @export
flag_duplicates.track_xyt <- function(x, gamma, time_unit = "mins",
                                      DOP = "dop", ...) {
  ## Check inputs
  # Check that x has rows (could have already been filtered)
  if (nrow(x) == 0) {
    warning("'x' has 0 rows. Check previous filtering.")
    x$duplicate_ <- logical(0)
    return(x)
  }
  # gamma
  if (!(is(gamma, "numeric") | is(gamma, "Period"))) {
    stop("'gamma' should be either 'numeric' or 'Period' from package 'lubridate'.")
  }
  # time_unit
  if (!is(gamma, "Period")) {
    check_time_unit(time_unit)
  }
  # DOP
  if(!is.character(DOP)) {
    stop("'DOP' should be a character string with the name of the DOP column.")
  }
  if(is.null(x[[DOP]])) {
    stop("Column '", DOP, "' does not exist in 'x'.")
  }

  ## Process inputs
  # Sort x by time
  x <- x[order(x$t_), ]

  # Give x a unique identifier
  # Choose a name unlikely to already be in use
  # This will correspond perfectly with the row numbers of x,
  # but will be useful for identifying flags later.
  x$UUID_ <- 1:nrow(x)

  # Check DOP column
  if(any(is.na(x[[DOP]]))) {
    stop("NAs found in column '", DOP, "'. ")
  }

  # Process gamma
  if (is(gamma, "Period")) {
    G <- as.numeric(gamma, unit = "secs")
  } else {
    # If gamma is numeric to begin with, make sure time unit is seconds
    if (time_unit == "secs") {
      G <- gamma
    } else {
      # Convert to seconds
      # Create period and then convert to seconds
      G <- as.numeric(lubridate::as.period(gamma, unit = time_unit), unit = "secs")
    }
  }

  # Duplicate x to allow row removal as flags are identified
  x2 <- x

  # Initialize flag column
  x$duplicate_ <- FALSE

  # Number of rows of data (updated as flagged rows are removed)
  n <- nrow(x)

  # Initialize i
  i <- 1

  while (i <= n) {
    # Points from time[i] to time[i] + G
    dups <- x2[(x2$t_ >= x2$t_[i]) & (x2$t_ <= (x2$t_[i] + G)), ]

    # Get row index
    good <- dups$UUID_[which(dups[[DOP]] == min(dups[[DOP]]))]

    # Keep dups in good
    best_dups <- dups[which(dups$UUID_ %in% good), ]

    # If there is more than 1, keep point closer to previous
    if (length(good) > 1) {
      # There is no previous point for i == 1
      if (i == 1) {
        best <- 1
      } else {

        # Find the point closest to previous point
        prev <- x2[i - 1, ]

        # Calculate the distance
        DIST <- sqrt((best_dups$x_ - prev$x_)^2 + (best_dups$y_ - prev$y_)^2)

        # Get the best
        best <- best_dups$UUID_[which(DIST == min(DIST))]

        # WHAT IF THERE'S ANOTHER TIE?
        # Take the first
        best <- best[1]
      }

    } else {
      # If there were no duplicates with same DOP
      best <- good
      # Increment i
      i <- i + 1
    }
    # Flag all but the best
    flag <- dups$UUID_[which(dups$UUID_ != best)]
    # Flag row(s) (in original x)
    x$duplicate_[flag] <- TRUE
    # Remove row(s) in copy
    x2 <- x2[which(!(x2$UUID_ %in% flag)), ]
    # Update n
    n <- nrow(x2)

  } # end while loop

  # Get rid of unique ID column
  x$UUID_ <- NULL

  # Return original data with flags
  return(x)
}

# Fast steps ----
#' Flag Fast Steps
#'
#' Flags locations that imply SDR exceeding a threshold
#'
#' @param x `[track_xyt]` A `track_xyt` object.
#' @param delta `[numeric]` The threshold SDR over which steps are flagged.
#' See details.
#' @param time_unit `[character]` Character string giving time unit. Should be
#' `"secs"`, `"mins"`, or `"hours"`. See details.
#' @param ... Addtional arguments. None currently implemented.
#'
#' @details Locations are flagged if the SDR from the previous location to the
#' current location exceeds `delta`. Internally, flagged locations are dropped
#' from future consideration.
#'
#' The `time_unit` should be the same time unit with which the SDR threshold
#' was calculated. SDR is typically calculated in `m^2/s`, so `time_unit` defaults
#' to `"secs"`. The spatial unit is determined by the CRS, which should typically
#' be in meters.
#'
#' @return Returns `x` (a `track_xyt`) with a flagging column added
#' (`x$fast_step_`).
#'
#' @seealso \code{\link{flag_duplicates}()}, \code{\link{flag_roundtrips}()},
#' \code{\link{flag_defunct_clusters}()}
#'
#' @author Brian J. Smith, based on code by Johannes Signer and Tal Avgar
#'
#' @name flag_fast_steps
#' @export
flag_fast_steps <- function(x, delta, time_unit = "secs", ...) {
  UseMethod("flag_fast_steps", x)
}

#' @rdname flag_fast_steps
#' @export
flag_fast_steps.track_xyt <- function(x, delta, time_unit = "secs", ...) {

  ## Check inputs
  # Check that x has rows (could have already been filtered)
  if (nrow(x) == 0) {
    warning("'x' has 0 rows. Check previous filtering.")
    x$fast_step_ <- logical(0)
    return(x)
  }
  # Duplicate timestamps
  if (any(duplicated(x$t_))) {
    stop("Duplicated timestamps. Remove temporal duplicates first.")
  }
  # delta
  checkmate::assert_numeric(delta)
  # time_unit
  check_time_unit(time_unit)

  # Sort x by time
  x <- x[order(x$t_), ]

  # Duplicate x to allow row removal as flags are identified
  x2 <- x

  # Initialize flag column
  x$fast_step_ <- FALSE

  # Number of rows of data (updated as flagged rows are removed)
  n <- nrow(x)

  # Initialize i
  i <- 2

  while (i <= n) {
    SDR <- ((x2$x_[i] - x2$x_[i - 1])^2 + (x2$y_[i] - x2$y_[i - 1])^2) /
      as.numeric(x2$t_[i] - x2$t_[i - 1], units = time_unit)

    # If SDR exceeds delta
    if (SDR > delta) {
      # Flag that row (in original x)
      x$fast_step_[i] <- TRUE
      # Remove that row in copy
      x2 <- x2[-i, ]
      # Update n
      n <- nrow(x2)

    } else {
      # If it didn't exceed delta
      # Increment i
      i <- i + 1
    }
  } # end while loop

  # Return original data with flags
  return(x)
}

# Fast roundtrips ----
#' Flag Fast Round Trips
#'
#' Flags locations that imply fast round trips
#'
#' @param x `[track_xyt]` A `track_xyt` object.
#' @param delta `[numeric]` The threshold SDR for flagging. Locations that imply
#' both legs of a round trip with `SDR > delta/epsilon` are flagged. See details.
#' @param epsilon `[numeric]` A scaling factor to create the threshold for
#' flagging.
#' @param time_unit `[character]` Character string giving time unit. Should be
#' `"secs"`, `"mins"`, or `"hours"`. See details.
#' @param ... Addtional arguments. None currently implemented.
#'
#' @details
#'
#' Locations implying a single fast step can be flagged using
#' \code{\link{flag_fast_steps}()}. However, it is more likely that a single
#' location is imprecise if it implies an unrealistically fast out-and-back round
#' trip. In that case, the user might be willing to scale the threshold SDR.
#' In this function, `delta` gives the base SDR and `epsilon` is the scaling
#' factor, such that locations are considered for flagging if the SDR from the
#' previous location (location i - 1) to the focal location (i) \[call it `sdr1`\]
#' and the focal location (i) to the next location (i + 1) \[call it `sdr2`] both
#' have `SDR > delta/epsilon`.
#'
#' In that case, the SDR from the previous location (i - 1) to the next location
#' (i + 1) is computed; i.e., the SDR assuming we omit the focal location (i)
#' \[call it `sdr3`\]. The remaining locations should be closer together than
#' they are to the omitted location. Thus the focal location is flagged if
#' `(sdr1 > epsilon * sdr3) & (sdr2 > epsilon * sdr3)`.
#'
#' Note that `epsilon` both _decreases_ `delta` in the out-and-back case and
#' _increases_ `sdr3` (between the remaining neighbors).
#'
#' Internally, flagged locations are dropped from future consideration.
#'
#' The `time_unit` should be the same time unit with which the SDR threshold
#' was calculated. SDR is typically calculated in `m^2/s`, so `time_unit` defaults
#' to `"secs"`. The spatial unit is determined by the CRS, which should typically
#' be in meters. The `epsilon` parameter is unitless.
#'
#' @return Returns `x` (a `track_xyt`) with a flagging column added
#' (`x$fast_roundtrip_`).
#'
#' @seealso \code{\link{flag_duplicates}()},
#' \code{\link{flag_fast_steps}()},
#' \code{\link{flag_defunct_clusters}()}
#'
#' @author Brian J. Smith, based on code by Johannes Signer and Tal Avgar
#'
#' @name flag_roundtrips
#' @export
flag_roundtrips <- function(x, delta, epsilon,
                            time_unit = "secs", ...) {
  UseMethod("flag_roundtrips", x)
}

#' @rdname flag_roundtrips
#' @export
flag_roundtrips.track_xyt <- function(x, delta, epsilon,
                                      time_unit = "secs", ...) {

  ## Check inputs
  # Check that x has rows (could have already been filtered)
  if (nrow(x) == 0) {
    warning("'x' has 0 rows. Check previous filtering.")
    x$fast_roundtrip_ <- logical(0)
    return(x)
  }
  # Duplicated timestamps
  if (any(duplicated(x$t_))) {
    stop("Duplicated timestamps. Remove temporal duplicates first.")
  }
  # delta
  checkmate::assert_numeric(delta)
  # epsilon
  checkmate::assert_numeric(epsilon)
  # time_unit
  check_time_unit(time_unit)

  # Sort x by time
  x <- x[order(x$t_), ]

  # Duplicate x to allow row removal as flags are identified
  x2 <- x

  # Initialize flag column
  x$fast_roundtrip_ <- FALSE

  # Number of rows of data (updated as flagged rows are removed)
  n <- nrow(x)

  # Initialize i
  i <- 2

  while(i < n) {
    # SDR for previous point (point 1) to focal point (point 2)
    dsq1 <- (x2$x_[i - 1] - x2$x_[i])^2 + (x2$y_[i - 1] - x2$y_[i])^2
    dt1 <- as.numeric(x2$t_[i] - x2$t_[i - 1], units = time_unit)
    sdr1 <- dsq1/dt1
    # SDR for focal point (point 2) to next step (point 3)
    dsq2 <- (x2$x_[i] - x2$x_[i + 1])^2 + (x2$y_[i] - x2$y_[i + 1])^2
    dt2 <- as.numeric(x2$t_[i + 1] - x2$t_[i], units = time_unit)
    sdr2 <- dsq2/dt2

    # If both exceed the delta/epsilon ratio
    if ((sdr1 > (delta / epsilon)) & (sdr2 > (delta / epsilon))) {
      # SDR for previous point (point 1) to next point (point 3)
      dsq3 <- (x2$x_[i - 1] - x2$x_[i + 1])^2 + (x2$y_[i - 1] - x2$y_[i + 1])^2
      dt3 <- as.numeric(x2$t_[i - 1] - x2$t_[i + 1], units = time_unit)
      sdr3 <- dsq3/dt3
    } else {
      sdr3 <- Inf
    }

    # Check whether it should be flagged
    if ((sdr1 > epsilon * sdr3) & (sdr2 > epsilon * sdr3)) {
      x$fast_roundtrip_[i] <- TRUE
      x2 <- x2[-i, ]
      n <- nrow(x2)
    } else {
      # If not, advance i
      i <- i + 1
    }
  }
  # Return
  return(x)
}

# Defunct clusters ----

#' Flag Defunct Clusters
#'
#' Flags defunct clusters at the end of a track
#'
#' @param x `[track_xyt]` A `track_xyt` object.
#' @param zeta `[numeric]` See details.
#' @param eta `[numeric]` See details.
#' @param theta `[numeric]` See details.
#' @param ... Addtional arguments. None currently implemented.
#'
#' @details
#'
#' Locations at the end of a trajectory may represent a dropped collar
#' or an animal mortality. In some cases, the device may be recording locations
#' for quite some time that are not biologically meaningful. This function
#' aims to flag those locations at the end of the trajectory that belong to a
#' mortality (or similar) cluster. The first location at the cluster remains
#' unflagged, but all subsequent locations are flagged.
#'
#' The algorithm detects steps that represent zero movement, within a precision
#' threshold given by `zeta`. That is, if `zeta = 5` (units determined by CRS;
#' typically meters), all points that differ by less than 5 will be considered
#' zero movement. Consecutive steps of zero movement (within the tolerance) form
#' a cluster. The parameter `eta` gives the cutoff for the minimum number of
#' zero steps to be considered a cluster. Finally, the algorithm requires that
#' clusters persist without a non-zero step for a minimum amount of time, given
#' by `theta`.
#'
#' @return Returns `x` (a `track_xyt`) with a flagging column added
#' (`x$defunct_cluster_`).
#'
#' @seealso \code{\link{flag_duplicates}()},
#' \code{\link{flag_fast_steps}()},
#' \code{\link{flag_roundtrips}()}
#'
#' @author Brian J. Smith and Johannes Signer, based on code by Tal Avgar
#'
#' @name flag_defunct_clusters
#' @export
flag_defunct_clusters <- function(x, zeta, eta, theta, ...) {
  UseMethod("flag_defunct_clusters", x)
}

# Note: almost entirely using Johannes' code for this one

#' @rdname flag_defunct_clusters
#' @export
flag_defunct_clusters.track_xyt <- function(x, zeta, eta, theta, ...) {

  ## Check inputs
  # Check that x has rows (could have already been filtered)
  if (nrow(x) == 0) {
    warning("'x' has 0 rows. Check previous filtering.")
    x$defunct_cluster_ <- logical(0)
    return(x)
  }
  # zeta
  checkmate::assert_numeric(zeta)
  # eta
  checkmate::assert_numeric(eta)
  # theta
  if(!is(theta, "Period")) {
    stop("'theta' should be a 'Period' from package 'lubridate'.")
  }

  # Sort x by time
  x <- x[order(x$t_), ]

  # Initialize flag column
  x$defunct_cluster_ <- FALSE

  # Calculate step lengths
  sl <- step_lengths(x, append_last = FALSE)

  # Get sequence of 0 steps using zeta as tolerance
  segs <- rle(sl <= zeta)

  if (any((segs$lengths + 1)[segs$values] > eta)) { # +1 to include the end point of the step

    # start
    start <- cumsum(c(1, head(segs$lengths, -1)))
    end <- cumsum(segs$lengths)
    w <- which(segs$lengths >= eta & segs$values)

    # check all segments, starting with the last
    for (i in rev(w)) {
      # Start segs
      s <- start[i]
      # End segs
      e <- end[i] + 1 # Because
      # now get the time difference
      if (as.numeric(difftime(x$t_[e], x$t_[s], units = "secs")) >
          as.numeric(theta, units = "secs")) {
        # flag all locations from `s` to end
        x$defunct_cluster_ <- TRUE
        x$defunct_cluster_[1:s] <- FALSE
        break()
      }
    }
  }
  return(x)
}
