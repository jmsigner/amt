#' Generate Random Points.
#'
#' Functions to generate random points within an animals home range. This is usually used for Resource Selection Functions (RSF).
#' @param x A track, or a home range.
#' @param n The number of random points.
#' @param type Argument passed to `sp::spsample type`.
#' @param level Home range level.
#' @param hr The home range estimator to be used.
#' @param factor How many random points shoud be estimated for each observed point?
#' @template dots_none
#' @name random_points
#' @export
random_points <- function(x, ...) {
  UseMethod("random_points", x)
}

#' @export
#' @rdname random_points
random_points.mcp <- function(x, n = 100, type = "random", ...) {
  as_track(sp::spsample(hr_isopleths(x), n = n, type = type, ...))
}

#' @export
#' @rdname random_points
random_points.track_xy <- function(x, level = 1, hr = "mcp", factor = 10, ...) {

  if (hr == "mcp") {
    hr <- hr_mcp(x, levels = level)
  } else {
    stop("Only mcp is currently implemented.")
  }

  rnd_pts <- random_points(hr, n = round(nrow(x)) * factor, type = "random", ...)

  n <- nrow(x)
  n_rnd <- nrow(rnd_pts)

  xx <- data_frame(
    case_ = c(rep(TRUE, n), rep(FALSE, n_rnd)),
    x_ = c(x$x_, rnd_pts$x_),
    y_ = c(x$y_, rnd_pts$y_)
  )
  class(xx) <- c("random_points", class(xx))
  xx
}

#' @export
#' @method plot random_points
plot.random_points <- function(x, y = NULL, ...) {
  with(x[!x$case_, ], plot(x_, y_, pch = 20, col = adjustcolor("black", 0.1),
                           asp = 1, xlab = "x", ylab = "y", las = 1))
  with(x[x$case_, ], points(x_, y_, pch = 20, col = "red"))
  legend("topright", pch = 20, col = c("red", adjustcolor("black", 0.1)),
         legend = c("observed", "random"))
}

const_call_random_points <- function(fun, data, ..., .dots) {
  cls <- attributes(data)
  class(data) <- class(data)[-1]
  out <- do.call(fun, list(data, ..., .dots))
  attributes(out)$class <- c(cls$class[1], class(out))
  attributes(out)$crs <- cls$crs
  out
}

# see here: https://github.com/hadley/dplyr/issues/719
#' @export
arrange_.random_points <- function(.data, ..., .dots) {
  const_call_random_points(dplyr::arrange_, .data, ..., .dots)
}

#' @export
filter_.random_points <- function(.data, ..., .dots) {
  const_call_random_points(dplyr::filter_, .data, ..., .dots)
}

#' @export
group_by_.random_points <- function(.data, ..., .dots) {
  const_call_random_points(dplyr::group_by_, data, ..., .dots)
}

#' @export
ungroup.random_points <- function(x, ...) {
  const_call_random_points(dplyr::ungroup, x, ..., NULL)
}

#' @export
distinct_.random_points <- function(.data, ..., .dots) {
  const_call_random_points(dplyr::distinct_, data, ..., .dots)
}

#' @export
select_.random_points <- function(.data, ..., .dots) {
  const_call_random_points(dplyr::select_, data, ..., .dots)
}

#' @export
summarize_.random_points <- function(.data, ..., .dots) {
  const_call_random_points(dplyr::summarize_, data, ..., .dots)
}

#' @export
summarise_.random_points <- function(.data, ..., .dots) {
  const_call_random_points(dplyr::summarise_, data, ..., .dots)
}

# tibble methods
#' @export
`[.random_points` <- function(x, i, j, drop = FALSE) {
  x <- NextMethod()
  class(x) <- c("random_points", class(x))
  x
}

