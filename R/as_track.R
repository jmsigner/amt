#' Coerce to track
#'
#' Coerce other classes (currently implemented: `SpatialPoints`) to a `track_xy`.
#' @export
#' @param x `[SpatialPoints]` \cr Object to be converted to a track.
#' @template dots_none
#' @name as_track
#' @examples
#' xy <- sp::SpatialPoints(cbind(c(1, 3, 2, 1), c(3, 2, 2, 1)))
#' as_track(xy)
as_track <- function(x, ...) {
  UseMethod("as_track", x)
}

#' @export
#' @rdname as_track
as_track.SpatialPoints <- function(x, ...) {
  xx <- sp::coordinates(x)
  track(x = xx[, 1], y = xx[, 2], crs = sp::proj4string(x))
}



#' @export
#' @rdname as_track
as_track.sfc_POINT <- function(x, ...) {
  xx <- sf::st_coordinates(x)
  track(x = xx[, 1], y = xx[, 2], crs = sf::st_crs(x))
}


#' @export
#' @rdname as_track
as_track.steps_xyt <- function(x, ...) {
  n <- nrow(x)

  crs <- get_crs(x)

  if ("burst_" %in% names(x)) {
    xx <- tidyr::nest(x, data = -c(burst_)) %>%
      dplyr::mutate(data = purrr::map(data, function(y) {
        n1 <- nrow(y)
        tibble::tibble(
          xs = c(y$x1_, y$x2_[n1]),
          ys = c(y$y1_, y$y2_[n1]),
          t = c(y$t1_, y$t2_[n1])
        )
      })) %>% tidyr::unnest(cols = data)
    make_track(xx, xs, ys, t, burst_ = burst_,
               crs = crs)
  } else {
    xx <- tibble::tibble(
      xs = c(x$x1_, x$x2_[n]),
      ys = c(x$y1_, x$y2_[n]),
      t = c(x$t1_, x$t2_[n])
    )
    make_track(xx, xs, ys, t, crs = crs)
  }

}
