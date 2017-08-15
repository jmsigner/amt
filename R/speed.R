#' Speed
#'
#' Obtain the speed of a track.
#'
#' @param x A `track_xyt`.
#' @param append_na `[logical(1)=TRUE]` \cr Should an `NA` be appended at the end.
#' @template dots_none
#' @return `[numeric]` \cr The speed in `m/s`.
#' @name speed
#' @export

speed <- function(x, ...) {
  UseMethod("speed", x)
}

#' @rdname speed
#' @export
speed.track_xyt <- function(x, append_na = TRUE, ...) {
  stps <- suppressWarnings(steps(x))
  s <- stps$sl_ / as.numeric(stps$dt_, units = "secs")
  if (append_na) {
    c(s, NA)
  } else {
    s
  }
}
