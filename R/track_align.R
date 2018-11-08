#' Selects relocations that fit a new time series
#'
#' Functions to only selects relocations that can be aligned with a new time series (within some tolerance).
#' @param x A track.
#' @param nt The new time trajectory.
#' @param tol The tolerance.
#' @template dots_none
#' @name track_align
#' @export

track_align <- function(x, ...) {
  UseMethod("track_align", x)
}

#' @rdname track_align
#' @export
track_align.track_xyt <- function(x, nt, tol, ...) {
  x[["burst_"]] <- track_align_raw(x, nt, tol, type = "burst")
  x[x$burst_ > -1, ] # -1 indicates that point is left out
}


track_align_raw <- function(x, nt, tol, type = "burst") {
  if (!type %in% c("which", "diff", "burst")) {
    stop("type should be one of: 'which', 'diff' or 'burst'.")
  }

  if (!lubridate::is.POSIXct(nt)) {
    stop("nt should be of class: POSIXct")
  }
  xx <- track_align_cpp(as.integer(x$t_), as.integer(nt), as.integer(lubridate::period_to_seconds(tol)),
                        switch(type,  which = 1L,  diff = 2L, burst = 3L))
}
