## TODO: rename to track_resample

# #' @export
#
# regular_sampling_rate_raw <- function(x, rate, tolerance, start = 1) {
#   xx <- regular_sampling_rate_raw(x, rate, tolerance, start)
# }

#' @export
track_resample <- function(x, ...) {
  UseMethod("track_resample", x)
}

#' @export
track_resample.track_xyt <- function(x, rate = hours(2), tolerance = minutes(15), start = 1, ...) {

  t_ <- as.numeric(x$t_)
  if (any(diff_rcpp(t_) < 0)) {
    stop("Neg. time diffs are not possible, maybe reorder?")
  }

  xx <- mk_reg(t1 = t_, time_dist = lubridate::period_to_seconds(rate),
         time_tol = lubridate::period_to_seconds(tolerance), start = start)
  x$burst_ <- xx
  cond <- quote(burst_ > 0) # -1 indicates that point is left out
  filter(x, cond)
}

#' @export
filter_min_n_burst <- function(x, ...) {
  UseMethod("filter_min_n_burst")
}

#' @export
filter_min_n_burst.track_xy <- function(x, min_n = 3, ...) {
  if (!"burst_" %in% names(x)) {
    stop("column 'burst_' not found.")
  }
  pred <- lazyeval::interp(~ col >= min_n, col = as.name("n"))
  x_select <- group_by_(x, "burst_") %>% summarise_(n = ~n()) %>%
    filter_(pred)
  #pred <- lazyeval::interp(~ col %in% x_select$burst_, col = as.name("burst_"))
  #x %>% filter_(pred)
  xx <- x[x$burst_ %in% x_select$burst_, ]
  class(xx) <- class(x)
  xx
}
