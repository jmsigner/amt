#' Movement metrics
#'
#' Functions to calculate metrics such as straightness, mean squared displacement (msd), intensity use,
#' sinuosity, mean turn angle correlation (`tac`) of a track.
#'
#' The intensity use is calculated by dividing the total movement distance (`tot_dist`) by the square of the area of movement (= minimum convex polygon 100).
#'
#' @template track_xy_star
#' @template dots_none
#' @name movement_metrics
#' @export
#' @references
#' \insertRef{abrahams2017}{amt}
#' \insertRef{almeida2010}{amt}
#' \insertRef{swihart1985}{amt}
#' @examples
#' data(deer)
#'
#' tot_dist(deer)
#' cum_dist(deer)
#' straightness(deer)
#' msd(deer)
#' intensity_use(deer)
#'

straightness <- function(x, ...) {
  UseMethod("straightness", x)
}


#' @export
straightness.track_xy <- function(x, ...) {
  tot_dist(x) / cum_dist(x)
}

#' @export
#' @rdname movement_metrics
cum_dist <- function(x, ...) {
  UseMethod("cum_dist", x)
}

#' @export
cum_dist.track_xy <- function(x, ...) {
  sum(sp::spDists(as.matrix(x[, c("x_", "y_")]), segments = TRUE))
}

#' @export
#' @rdname movement_metrics
tot_dist <- function(x, ...) {
  UseMethod("tot_dist", x)
}

#' @export
tot_dist.track_xy <- function(x, ...) {
  sp::spDists(as.matrix(x[c(1, nrow(x)), c("x_", "y_")]), segments = TRUE)
}

#' @export
#' @rdname movement_metrics
msd <- function(x, ...) {
  UseMethod("msd", x)
}

#' @export
msd.track_xy <- function(x, ...) {
  mx <- mean(x$x_)
  my <- mean(x$y_)
  mean((x$x_ - mx)^2 + (x$y_ - my)^2)
}

#' @export
#' @rdname movement_metrics
intensity_use <- function(x, ...) {
  UseMethod("intensity_use")
}

#' @export
intensity_use.track_xy <- function(x, ...) {
  cum_dist(x) / hr_area(hr_mcp(x, levels = 1))$area^0.5
}

#' @export
#' @rdname movement_metrics
sinuosity <- function(x, ...) {
  UseMethod("sinuosity")
}

#' @export
sinuosity.track_xy <- function(x, ...) {
  x <- steps(x)
  p <- mean(x$sl_)
  cv <- sd(x$sl_) / p
  mc <- mean(cos(x$ta_ * pi/180), na.rm = TRUE)
  ms <- mean(sin(x$ta_ * pi/180), na.rm = TRUE)
  2 * (p * ((1 - mc^2 - ms^2) / ((1 - mc)^2 + ms^2) + cv^2))^(-0.5)
}


#' @export
#' @rdname movement_metrics
tac <- function(x, ...) {
  UseMethod("tac")
}

#' @export
tac.track_xy <- function(x, ...) {
  # following abrahams 2017 and dray 2010
  x <- steps(x)
  ta <- x$ta_[-1] * pi/180
  1/nrow(x) * sum(diff(cos(ta))^2 + diff(cos(ta))^2)


}

