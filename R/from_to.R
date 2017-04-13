#' @export
from_to <- function(x, ...) {
  UseMethod("from_to", x)
}

#' @export
from_to.track_xyt <- function(x, ...) {
  c(from(x), to(x))
}

#' @export
from <- function(x, ...) {
  UseMethod("from", x)
}

#' @export
from.track_xyt <- function(x, ...) {
  min(x$t_)
}

#' @export
to <- function(x, ...) {
  UseMethod("to", x)
}

#' @export
to.track_xyt <- function(x, ...) {
  max(x$t_)
}

