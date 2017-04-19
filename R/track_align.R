track_align_raw <- function(x, nt, tol, type = "burst") {
    if (!type %in% c("which", "diff", "burst")) {
      stop("type should be one of: 'which', 'diff' or 'burst'.")
    }

    if (!is.POSIXct(nt)) {
      stop("nt should be of class: POSIXct")
    }
    xx <- track_align(as.numeric(x$t_), as.numeric(nt), lubridate::period_to_seconds(tol),
                      switch(type,  which = 1,  diff = 2, burst = 3))
}

#' @export
track_align <- function(x, ...) {
  UseMethod("track_align", x)
}

#' @export
track_align.track_xyt <- function(x, nt, tol, ...) {
  x[["burst_"]] <- track_align_raw(x, nt, tol, type = "burst")
  cond <- quote(burst_ > -1) # -1 indicates that point is left out
  filter_(x, cond)
}

