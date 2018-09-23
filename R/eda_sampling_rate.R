#' Returns a summary of sampling rates
#'
#'
#' @param x A `track_xyt`.
#' @param time_unit A character. The time unit in which the sampling rate is
#'   calculated. Acceptable values are `sec`, `min`, `hour`, `day`, and `auto`.
#'   If `auto` (the default) is used, the optimal unit is guessed.
#' @param summarize A logical. If `TRUE` a summary is returned, otherwise raw
#'   sampling intervals are returned.
#' @param as_tibble A logical. Should result be returned as `tibble` or as
#'   `table`.
#' @template dots_none
#'
#' @return Depending on `summarize` and `as_tibble`, a vector, table or tibble.
#' @export
#'
#' @name summarize_sampling_rate
#' @examples
#' data(deer)
#' amt::summarize_sampling_rate(deer)
#'
summarize_sampling_rate <- function(x, ...) {
  UseMethod("summarize_sampling_rate", x)
}

#' @rdname summarize_sampling_rate
#' @export
summarize_sampling_rate.track_xyt <- function(x, time_unit = "auto", summarize = TRUE, as_tibble = TRUE, ...) {

  t_diff <- diff(as.numeric(x$t_))
  m_t_diff <- median(t_diff)

  sec_day <- 60 * 60 * 24
  sec_hour <- 60 * 60
  sec_min <- 60

  opt_time <- if (time_unit == "auto") {
    if (m_t_diff / sec_day < 1) {
      if (m_t_diff / sec_hour < 1) {
        if (m_t_diff / sec_min < 1) {
          "sec"
        } else {
          "min"
        }
      } else {
        "hour"
      }
    } else {
      "day"
    }
  } else {
    time_unit
  }

  # Convert time units
  t_diff <- if (opt_time == "day") {
    t_diff / sec_day
  } else if (opt_time == "hour") {
    t_diff / sec_hour
  } else if (opt_time == "min") {
    t_diff / sec_min
  } else {
    t_diff
  }

  if (summarize) {
    t_diff_s <- summary(t_diff)
    if (as_tibble) {
      data_frame(
        min = t_diff_s[1],
        q1 = t_diff_s[2],
        median = t_diff_s[3],
        mean = t_diff_s[4],
        q3 = t_diff_s[5],
        max = t_diff_s[6],
        sd = sd(t_diff),
        n = length(t_diff),
        unit = opt_time
      )
    }
  } else {
    t_diff
  }

}


