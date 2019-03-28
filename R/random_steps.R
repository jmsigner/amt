#' Generate Random Steps
#'
#' Function to generate a given number of random steps for each observed step.
#'
#' @param x Steps.
#' @param n_control `[integer(1)=10]{>1}` \cr The number of control steps paired
#'   with each observed step.
#' @param sl_distr `[amt_distr]` \cr The step-length distribution.
#' @param ta_distr `[amt_distr]` \cr The turn-angle distribution.
#' @param random.error `[numeric(1)=0.001]{>0}` \cr Upper limit for a uniformly
#' @template dots_none
#' @export
#' @name random_steps
random_steps2 <- function(x, ...) {
  UseMethod("random_steps", x)
}

#' @export
#' @rdname random_steps
random_steps.steps_xy <- function(
  x, n_control = 10,
  sl_distr = fit_distr(sl_, "gamma"),
  ta_distr = fit_distr(ta_, "vonmises"),
  rand_sl = random_numbers(sl_distr, n = 1e5),
  rand_ta = random_numbers(ta_distr, n = 1e5),
  include_observed = TRUE,
  include_
  remove_first = TRUE,  ...) {


  ###
  library(amt)
  data(deer)
  x <- steps_by_burst(deer)
  x <- x[1:3, ]
  head(deer)
  xx <- x[1:3, ] %>% random_steps(n_control = 2)
  head(deer)
  n_control <- 3

  sl_distr = make_exp_distr(rate = 0.03)
  ta_distr = make_unif_distr()

  slr <- random_numbers(sl_distr, n = n_sample_available) # can become an arg
  tar <- random_numbers(ta_distr, n = n_sample_available) # can become an arg

  ###
  # Generate random points
  ns <- nrow(x)  # number of steps
  case_for_control <- rep(1:ns, each = n_control)

  rs <- (random_steps_cpp(n_control, x$x1_, x$y1_, x$x2_ , x$y2_, slr, tar, include_obs = 1, sl_obs = x$sl_,
                          ta_obs = x$ta_))

  rs
  x
  as.matrix(x)
      as.matrix(x[, c("x1_", "y1_", "x2_", "y2_", "ta_")])
      x

  if (include_obs) {
    rbind(
      x

    )
    rs$case_ <- FALSE
    xy_cc$step_id_ <- rep(1:ns, each = n_control)
    xy_cc

    x$case_ <- TRUE
    x$step_id_ <- 1:ns
    x


  has_burst <- "burst_" %in% names(x)

  vars <- c("step_id_", "case_", "x1_", "y1_", "x2_", "y2_")
  if (is(x, "steps_xyt")) {
    vars <- c(vars, "t1_", "t2_", "dt_")
  }

  if (has_burst) {
    vars <- c("burst_", vars)
  }

  }



  # shuffle attributes in non_vars
  v1 <- base::setdiff(names(x), vars)
  # xy_cc <- xy_cc %>% mutate_at(v1, function(x) sample(unique(x), length(x), TRUE))
  # message("shuffling non standard columns")
  vars <- c(vars, v1)

  suppressWarnings(out <- dplyr::bind_rows(x, xy_cc))
  out
  library(rlang)
  out <- dplyr::arrange(out, !!quo(step_id_))
  out <- dplyr::select(out, vars)

  class(out) <- c("random_steps", class(out))
  attributes(out)$sl_ <- sl
  attributes(out)$ta_ <- ta
  attributes(out)$n_control_ <- n_control
  out

  attr(xx, "crs_") <- attr(x, "crs_")
  xx
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
select.random_steps <- function(.data, ..., .dots) {
  xx <- NextMethod()
  rsteps_transfer_attr(.data, xx)
}

#' @export
summarise.random_steps <- function(.data, ..., .dots) {
  xx <- NextMethod()
  rsteps_transfer_attr(.data, xx)
}


#' @export
summarize.random_steps <- function(.data, ..., .dots) {
  xx <- NextMethod()
  rsteps_transfer_attr(.data, xx)
}
