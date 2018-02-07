#' Removes Capture Effects
#'
#' Removing relocations at the beginning and/or end of a track, that fall within a user specified period.
#'
#' @param x An object of class `track_xyt`.
#' @template dots_none
#' @export
#' @name remove_capture
#' @examples
#' library(lubridate)
#' n <- 10
#' df <- track(
#'   x = cumsum(rnorm(n)),
#'   y = cumsum(rnorm(n)),
#'   t = ymd_hm("2017-01-01 00:00") +
#'    hours(seq(0, by = 24, length.out = n))
#' )
#'
#' df
#' remove_capture_effect(df, start = days(1))
#' remove_capture_effect(df, end = days(2))
#' remove_capture_effect(df, start = days(1), end = days(2))
#'

remove_capture_effect <- function(x, ...) {
  UseMethod("remove_capture_effect", x)
}

#' @export
#' @param start A `lubirdate::Period`, indicating the time period to be removed at the beginning of the track.
#' @param end A `lubirdate::Period`, indicating the time period to be removed at the end of the track.
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

