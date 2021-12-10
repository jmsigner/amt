#' Removes Capture Effects
#'
#' Removing relocations at the beginning and/or end of a track, that fall within a user specified period.
#'
#' @param x An object of class `track_xyt`.
#' @template dots_none
#' @param start A `lubirdate::Period`, indicating the time period to be removed at the beginning of the track.
#' @param end A `lubirdate::Period`, indicating the time period to be removed at the end of the track.
#' @return A `tibble` without observations that fall within the period of the capture effect.
#' @export
#' @name remove_capture
remove_capture_effect <- function(x, ...) {
  UseMethod("remove_capture_effect", x)
}

#' @export
#' @rdname remove_capture
remove_capture_effect.track_xyt <- function(x, start, end, ...) {
  t <- as.numeric(x$t_)
  if (missing(end)) {
    if (lubridate::is.period(start)) {
      cond <- !t < t[1] + lubridate::period_to_seconds(start)
      filter(x, cond)
    } else {
      stop("start no lubridate::Period")
    }
  } else if (missing(start)) {
    if (lubridate::is.period(end)) {
      cond <- !t > t[length(t)] - lubridate::period_to_seconds(end)
      filter(x, cond)
    } else {
      stop("start no lubridate::Period")
    }
  } else {
    if (lubridate::is.period(start) & lubridate::is.period(start)) {
      cond <- rm_both(t, time_span = c(lubridate::period_to_seconds(start), lubridate::period_to_seconds(end)))
      filter(x, cond)
    } else {
      stop("start no lubridate::Period")
    }
  }
}

rm_both <- function(x, time_span) {
  tt <- time_span
  if (length(tt) < 2) tt <- rep(tt, 2)
  (!x < (x[1] + tt[1]) & !x > (x[length(x)] - tt[2]))
}

