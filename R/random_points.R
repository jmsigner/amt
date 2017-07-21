#' Generate random points
#'
#' Functions to generate random points within an animals home range. This is usually the first step for investigating habitat selection via Resource Selection Functions (RSF).
#' @template track_xy_star
#' @param n `[integer(1)]` \cr The number of random points.
#' @param type `[character(1)]` \cr Argument passed to `sp::spsample type`. The default is `random`.
#' @param level `[numeric(1)]` \cr Home-range level of the minimum convex polygon, used for generating the background samples.
#' @param hr `[character(1)]` \cr The home range estimator to be used. Currently only MCP is implemented.
#' @param factor `[numeric(1)]` Determines the number of random points that are generated. If `factor == 1` the number of presence points is equal to the number of observed points.
#' @param ... `[any]`\cr None implemented.
#' @note For objects of class `track_xyt` the timestamp (`t_`) is lost.
#' @name random_points
#' @export
#' @examples
#'
#' data(deer)
#'
#' # track_xyt ---------------------------------------------------------------
#' # Default settings
#' rp <- random_points(deer)
#' \dontrun{
#' plot(rp)
#' }
#' # Only one random point for each observed point
#' rp <- random_points(deer, factor = 1)
#' \dontrun{
#' plot(rp)
#' }
#'
#' # Within a home range -----------------------------------------------------
#' hr <- hr_mcp(deer, level = 1)
#'
#' # 100 random point within the home range
#' rp <- random_points(hr, n = 100)
#' \dontrun{
#' plot(rp)
#' }
#'
#' # 100 regular point within the home range
#' rp <- random_points(hr, n = 100, type = "regular")
#' \dontrun{
#' plot(rp)
#' }
#' # 100 hexagonal point within the home range
#' rp <- random_points(hr, n = 100, type = "hexagonal")
#' \dontrun{
#' plot(rp)
#' }
#'
random_points <- function(x, ...) {
  UseMethod("random_points", x)
}

#' @export
random_points.default <- function(x, ...) {
  stop("No methode random_points is implemented for this kind of objects.")
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
arrange.random_points <- function(.data, ..., .dots) {
  const_call_random_points(dplyr::arrange_, .data, ..., .dots)
}

#' @export
filter.random_points <- function(.data, ..., .dots) {
  const_call_random_points(dplyr::filter_, .data, ..., .dots)
}

#' @export
group_by.random_points <- function(.data, ..., .dots) {
  const_call_random_points(dplyr::group_by_, data, ..., .dots)
}

#' @export
ungroup.random_points <- function(x, ...) {
  const_call_random_points(dplyr::ungroup, x, ..., NULL)
}

#' @export
distinct.random_points <- function(.data, ..., .dots) {
  const_call_random_points(dplyr::distinct_, data, ..., .dots)
}

#' @export
select.random_points <- function(.data, ..., .dots) {
  const_call_random_points(dplyr::select_, data, ..., .dots)
}

#' @export
summarize.random_points <- function(.data, ..., .dots) {
  const_call_random_points(dplyr::summarize_, data, ..., .dots)
}

#' @export
summarise.random_points <- function(.data, ..., .dots) {
  const_call_random_points(dplyr::summarise_, data, ..., .dots)
}

# tibble methods
#' @export
`[.random_points` <- function(x, i, j, drop = FALSE) {
  x <- NextMethod()
  class(x) <- c("random_points", class(x))
  x
}

