#' Check for Capture Effects.
#'
#' @param x A \code{data_table} .
#' @param capture_effect_duration A an object of class period, indicating the length of putative capture effects.
#'
#' @return
#' @export
#'
#' @name capture_effect
#' @aliases rm_start
#' @aliases rm_end
#' @aliases rm_both
#'
#' @examples
#' library(lubridate)
#' n <- 1e1
#' df <- data_frame(
#'   id = 1,
#'   x_ = cumsum(rnorm(n)),
#'   y_ = cumsum(rnorm(n)),
#'   t_ = ymd_hm("2017-01-01 00:00") + hours(seq(0, by = 12, length.out = n))
#' )
#'
#' rm_capture_effect(df, start = days(1))
#' rm_capture_effect(df, end = days(2))
#' rm_capture_effect(df, start = days(1), end = days(2))
#'
#' df %>% head_tail(n = 4)
#'
#' df %>% filter(rm_start(df, time_span = days(10))) %>% head
#' df %>% filter(rm_end(df, time_span = days(10))) %>% tail
#' df %>% filter(rm_both(df, time_span = days(10))) %>% head_tail(n = 4)


#' @export
#' @rdname capture_effect

rm_capture_effect <- function(x, start, end) {
  if (missing(end))
    x %>% filter(rm_start(., time_span = start))
  else if (missing(start))
    x %>% filter(rm_end(., time_span = end))
  else
    x %>% filter(rm_both(., time_span = c(start, end)))
}

#' @export
#' @rdname capture_effect
rm_start <- function(x, time_span = lubridate::days(1)) {
  xx <- track2t_(x, time_span)
  !xx < xx[1] + lubridate::period_to_seconds(time_span)[1]
}

#' @export
#' @rdname capture_effect
rm_end <- function(x, time_span = lubridate::days(1)) {
  xx <- track2t_(x, time_span)
  !xx > xx[length(xx)] - lubridate::period_to_seconds(time_span)[1]
}

#' @export
#' @rdname capture_effect
rm_both <- function(x, time_span = lubridate::days(1)) {
  xx <- track2t_(x, time_span)
  tt <- lubridate::period_to_seconds(time_span)
  if (length(tt) < 2) tt <- rep(tt, 2)
  (!xx < (xx[1] + tt[1]) & !xx > (xx[length(xx)] - tt[2]))
}

track2t_ <- function(x, time_span)  {
  if (check_trackXYT(x)) {
    if (all(lubridate::is.period(time_span))) {
      xx <- as.numeric(x$t_)
    } else {
      stop("time span, no object of class period.")
    }
  }
}
