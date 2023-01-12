#' Coerce to track
#'
#' Coerce other classes to a `track_xy`.
#' @export
#' @param x Object to be converted to a track.
#' @template dots_none
#' @name as_track
#' @return An object of class `track_xy(t)`
as_track <- function(x, ...) {
  UseMethod("as_track", x)
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
    xx <- tidyr::nest(x, data = -c(burst_)) |>
      dplyr::mutate(data = purrr::map(data, function(y) {
        n1 <- nrow(y)
        tibble::tibble(
          xs = c(y$x1_, y$x2_[n1]),
          ys = c(y$y1_, y$y2_[n1]),
          t = c(y$t1_, y$t2_[n1])
        )
      })) |> tidyr::unnest(cols = data)
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

# Thanks to bniebuhr see https://github.com/jmsigner/amt/issues/44
#' @export
#' @rdname as_track
as_track.data.frame <- function(x, ...) {
  cols <- colnames(x)
  if("x_" %in% cols & "y_" %in% cols & !("t_" %in% cols)) {
    make_track(x, .x = x_, .y = y_, ...)
  } else {
    if("x_" %in% cols & "y_" %in% cols & "t_" %in% cols) {
      make_track(x, .x = x_, .y = y_, .t = t_, ...)
    }
  }

}
