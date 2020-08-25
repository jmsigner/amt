#' Generate Random Steps
#'
#' Function to generate a given number of random steps for each observed step.
#'
#' @param x Steps.
#' @param n_control `[integer(1)=10]{>1}` \cr The number of control steps paired
#'   with each observed step.
#' @param sl_distr `[amt_distr]` \cr The step-length distribution.
#' @param ta_distr `[amt_distr]` \cr The turn-angle distribution.
#' @param angle `[numeric(1) = 0]{-pi < rel_angle < pi}` \cr Angle for the first step.
#' @param  rand_sl `[numeric]` \cr Numeric vector with random step lengths an animal can make. This will usually be random numbers drawn from a suitable distribution (e.g., gamma or exponential).
#' @param  rand_ta `[numeric]` \cr Numeric vector with relative turning angles an animal can make. This will usually be random numbers drawn from a suitable distribution (e.g., von Mises or uniform).
#' @param include_observed `[logical(1) = TRUE]` \cr Indicates if observed steps are to be included in the result.
#' @template dots_none
#' @export
#' @name random_steps
random_steps <- function(x, ...) {
  UseMethod("random_steps", x)
}

#' @export
#' @rdname random_steps
random_steps.numeric <- function(
  x, n_control = 10,
  angle = 0,
  rand_sl = random_numbers(make_exp_distr(), n = 1e5),
  rand_ta = random_numbers(make_unif_distr(), n = 1e5), ...) {

  # Check arguments
  checkmate::assert_numeric(x, len = 2)
  checkmate::assert_int(n_control, lower = 1)
  checkmate::assert_numeric(rand_sl, lower = 0)
  checkmate::assert_numeric(rand_ta, lower = -pi, upper = pi)

  rs <- random_steps_cpp_one_step(
    n_control,  # number of controll steps
    x[1], x[2],
    angle,
    rand_sl, rand_ta)
  rs
}


#' @export
#' @rdname random_steps
#'
random_steps.steps_xy <- function(
  x, n_control = 10,
  sl_distr = fit_distr(x$sl_, "gamma"), # this argument could be remove
  ta_distr = fit_distr(x$ta_, "vonmises"), # this argument could be remove
  rand_sl = random_numbers(sl_distr, n = 1e5),
  rand_ta = random_numbers(ta_distr, n = 1e5),
  include_observed = TRUE, ...) {


  # Generate random points
  ns <- nrow(x)  # number of steps
  case_for_control <- rep(1:ns, each = n_control)

  stps <- which(!is.na(x$direction_p))
  x$step_id_ <- 1:nrow(x)
  x$case_ <- TRUE

  # This could be moved to c++
  xx <- lapply(stps, function(i) {
    random_steps(c(x$x1_[i], x$y1_[i]), n_control = n_control,
                 angle = x$direction_p[i],
                 rand_sl = rand_sl, rand_ta = rand_ta)})
 xx <- do.call(rbind, xx)

 for_rand <- x[rep(stps, each = n_control), ]
 for_rand$case_ <- FALSE

 for_rand$x2_ <- xx[, "x2_"]
 for_rand$y2_ <- xx[, "y2_"]

 for_rand$sl_ <- xx[, "sl_"]
 for_rand$ta_ <- xx[, "ta_"]
 x <- x[stps, ]

 out <- dplyr::bind_rows(x, for_rand)
 out <- dplyr::arrange(out, step_id_)

 class(out) <- c("random_steps", class(out))
 attributes(out)$sl_ <- sl_distr
 attributes(out)$ta_ <- ta_distr
 attributes(out)$n_control_ <- n_control
 attr(out, "crs_") <- attr(x, "crs_")

 out

}

#' @export
plot.random_steps <- function(x, ...) {
  plot(0, 0, type = "n",
       xlim = grDevices::extendrange(c(x$x1_, x$x2_), f = 0.1),
       ylim = grDevices::extendrange(c(x$y1_, x$y2_), f = 0.1),
       xlab = "x", ylab = "y")
  for (i in 1:nrow(x)) {
    graphics::lines(c(x$x1_[i], x$x2_[i]), c(x$y1_[i], x$y2_[i]), lty = 2,
                    col = "grey79")
  }

  x1 <- x[x$case_, ]
  for (i in 1:nrow(x1)) {
    lines(c(x1$x1_[i], x1$x2_[i]), c(x1$y1_[i], x1$y2_[i]))
    graphics::points(x1$x1_[i], x1$y1_[i], pch = 20, cex = 2, col = "red")
    graphics::points(x1$x2_[i], x1$y2_[i], pch = 20, cex = 2, col = "red")
  }
}


rsteps_transfer_attr <- function(from, to) {
  from <- attributes(from)
  attributes(to)$class <- from$class
  attributes(to)$sl_ <- from$sl_
  attributes(to)$ta_ <- from$ta_
  attributes(to)$crs_ <- from$crs_
  to
}



# see here: https://github.com/hadley/dplyr/issues/719
#' @export
arrange.random_steps <- function(.data, ..., .dots) {
  xx <- NextMethod()
  rsteps_transfer_attr(.data, xx)
}


#' @export
filter.random_steps <- function(.data, ..., .dots) {
  xx <- NextMethod()
  rsteps_transfer_attr(.data, xx)
}

#' @export
group_by.random_steps <- function(.data, ..., .dots) {
  xx <- NextMethod()
  rsteps_transfer_attr(.data, xx)
}

#' @export
nest.random_steps <- function(.data, ..., .dots) {
  NextMethod()
}

#' @export
select.random_steps <- function(.data, ..., .dots) {
  xx <- NextMethod()
  rsteps_transfer_attr(.data, xx)
}

#' @export
summarise.random_steps <- function(.data, ..., .dots) {
  NextMethod()
}


#' @export
summarize.random_steps <- function(.data, ..., .dots) {
  NextMethod()
}
