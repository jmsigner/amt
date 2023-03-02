#' Resample track
#'
#' Function to resample a track at a predefined sampling rate within some tolerance.
#'
#' @param x A `track_xyt`.
#' @param rate A lubridate `Period`, that indicates the sampling rate.
#' @param tolerance A lubridate `Period`, that indicates the tolerance of deviations of the sampling rate.
#' @param start A integer scalar, that gives the relocation at which the sampling rate starts.
#' @template dots_none
#' @name track_resample
#' @return A resampled `track_xyt`.
#' @export
track_resample <- function(x, ...) {
  UseMethod("track_resample", x)
}

#' @export
#' @rdname track_resample
track_resample.track_xyt <- function(x, rate = hours(2), tolerance = minutes(15), start = 1, ...) {

  t_ <- as.numeric(x$t_)
  if (any(diff(t_) < 0)) {
    stop("Neg. time diffs are not possible, maybe reorder?")
  }

  xx <- mk_reg(t1 = t_, time_dist = lubridate::period_to_seconds(rate),
         time_tol = lubridate::period_to_seconds(tolerance), start = start)
  x$burst_ <- xx
 # cond <- quo(burst_ > 0) # -1 indicates that point is left out
  filter(x, !!quo(burst_ > 0))
}


mk_reg <- function(t1, time_dist, time_tol, start) {

  n <- length(t1)
  out <- numeric(n)
  k <- 1

  if (start > 1) {
    out[1:(start - 1)] <- -1
  }
  out[start] <- 1

  i <- start
  while(i != n) {
    t_min = t1[i] + time_dist - time_tol
    t_max = t1[i] + time_dist + time_tol
    j <- i + 1
    while((j < n) && (t1[j] < t_min)) {
      out[j] <- -1
      j <- j + 1
    }
    i <- j
    if ((j == n) && (t1[j] < t_min)) {
      out[j] = -1
    } else if (t1[j] >= t_min && t1[j] <= t_max) {
      out[j] = k
    } else {
      k <- k + 1
      out[j] = k
    }
  }
  out
}

#' Filter bursts by number of relocations
#'
#' Only retain bursts with a minimum number (= `min_n`) of relocations.
#'
#' @template track_xy_star
#' @param min_n `[numeric(1)=3]` \cr Indicating the minimum number of relocations (=fixes per burst).
#' @template dots_none
#' @name filter_min_n_burst
#' @return A `tibble` of class `track_xy(t)`.
#' @export
filter_min_n_burst <- function(x, ...) {
  UseMethod("filter_min_n_burst")
}

#' @export
#' @rdname filter_min_n_burst
filter_min_n_burst.track_xy <- function(x, min_n = 3, ...) {

  if (!"burst_" %in% names(x)) {
    stop("column 'burst_' not found.")
  }
  #pred <- lazyeval::interp(~ col >= min_n, col = as.name("n"))
  x_select <- group_by(x, !!quo(burst_)) |> summarise(n = n()) |>
    filter(!!quo(n >= min_n))
  #pred <- lazyeval::interp(~ col %in% x_select$burst_, col = as.name("burst_"))
  #x |> filter_(pred)
  xx <- x[x$burst_ %in% x_select$burst_, ]
  class(xx) <- class(x)
  xx
}

