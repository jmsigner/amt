#' Generate random points
#'
#' Functions to generate random points within an animals home range. This is usually the first step for investigating habitat selection via Resource Selection Functions (RSF).
#' @template track_xy_star
#' @param n `[integer(1)]` \cr The number of random points.
#' @param type `[character(1)]` \cr Argument passed to `sp::spsample type`. The default is `random`.
#' @param level `[numeric(1)]` \cr Home-range level of the minimum convex polygon, used for generating the background samples.
#' @param hr `[character(1)]` \cr The home range estimator to be used. Currently only MCP is implemented.
#' @param presence `[track]` \cr The presence points, that will be added to the result.
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
#' rp1 <- random_points(deer)
#'
#' \dontrun{
#' plot(rp1)
#' }
#'
#'  \dontrun{
#' trast <- raster(bbox(deer, buffer = 5000), res = 30)
#' rp3 <- random_points(deer, hr = "kde", trast = trast) # we need a larger template raster
#'
#' plot(rp3)
#' }
#'
#'
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
random_points.hr <- function(x, n = 100, type = "random", presence = NULL, ...) {
  random_points_base(poly = hr_isopleths(x, ...), presence = presence,
                     type = type, n = n)
}

#' @export
#' @rdname random_points
random_points.sf <- function(x, n = 100, type = "random", presence = NULL, ...) {
  random_points_base(poly = x, presence = presence,
                     type = type, n = n)
}

#' @export
#' @rdname random_points
random_points.SpatialPolygons <- function(x, n = 100, type = "random", presence = NULL, ...) {
  as_track(sp::spsample(x, n = n, type = type, ...))
  random_points_base(poly = sf::st_as_sf(x), presence = presence,
                     type = type, n = n)
}

#' @export
#' @rdname random_points
random_points.track_xy <- function(x, level = 1, hr = "mcp", n = nrow(x) * 10, type = "random", ...) {

  if (hr == "mcp") {
    hr <- hr_mcp(x, levels = level, ...) %>% hr_isopleths()
  } else if (hr == "kde") {
    hr <- hr_kde(x, ...) %>% hr_isopleths(level = level)
  } else {
    stop("Only mcp and kde home ranges are currently implemented.")
  }
  random_points_base(poly = hr, presence = x, type = type, n = n)
}



random_points_base <- function(poly, presence, type, n, ...) {

  rnd_pts <- sf::st_coordinates(
    sf::st_sample(poly, size = n, type = type, ...))

  xx <- tibble(
    case_ = FALSE,
    x_ = rnd_pts[, 1],
    y_ = rnd_pts[, 2]
  )

  if (!is.null(presence)) {
    stopifnot(inherits(presence, "track_xy"))

    xx <- dplyr::bind_rows(
      xx,
      tibble(
        case_ = TRUE,
        x_ = presence$x_,
        y_ = presence$y_
      ))
  }

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

rp_transfer_attr <- function(from, to) {
  from <- attributes(from)
  attributes(to)$class <- c(setdiff(from$class, class(to)), class(to))
  attributes(to)$crs_ <- from$crs_
  to
}


#' @export
`[.random_points` <- function(x, i, j, drop = FALSE) {
  xx <- NextMethod()
  rp_transfer_attr(x, xx)
}

#' @export
arrange.random_points <- function(.data, ..., .dots) {
  xx <- NextMethod()
  rp_transfer_attr(.data, xx)
}

#' @export
filter.random_points <- function(.data, ..., .dots) {
  xx <- NextMethod()
  rp_transfer_attr(.data, xx)
}

#' @export
group_by.random_points <- function(.data, ..., .dots) {
  xx <- NextMethod()
  rp_transfer_attr(.data, xx)
}

#' @export
nest.random_points <- function(.data, ..., .dots) {
  NextMethod()
}

#' @export
ungroup.random_points <- function(x, ...) {
  xx <- NextMethod()
  rp_transfer_attr(.data, xx)
}

#' @export
distinct.random_points <- function(.data, ..., .dots) {
  xx <- NextMethod()
  rp_transfer_attr(.data, xx)
}

#' @export
select.random_points <- function(.data, ..., .dots) {
  xx <- NextMethod()
  rp_transfer_attr(.data, xx)
}

#' @export
summarize.random_points <- function(.data, ..., .dots) {
  NextMethod()
}

#' @export
summarise.random_points <- function(.data, ..., .dots) {
  NextMethod()
}

