#' Duration of tracks
#'
#' Function that returns the start (`from`), end (`to`), and the duration (`from_to`) of a track.
#' @template track_xy_star
#' @template dots_none
#' @name from_to
#' @export
#' @examples
#' data(deer)
#' from(deer)
#' to(deer)
#' from_to(deer)

from_to <- function(x, ...) {
  UseMethod("from_to", x)
}

#' @export
#' @rdname from_to
from_to.track_xyt <- function(x, ...) {
  c(from(x), to(x))
}

#' @export
#' @rdname from_to
from <- function(x, ...) {
  UseMethod("from", x)
}

#' @export
#' @rdname from_to
from.track_xyt <- function(x, ...) {
  min(x$t_)
}

#' @export
#' @rdname from_to
to <- function(x, ...) {
  UseMethod("to", x)
}

#' @export
#' @rdname from_to
to.track_xyt <- function(x, ...) {
  max(x$t_)
}

