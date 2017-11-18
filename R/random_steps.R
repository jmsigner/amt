#' Generate Random Steps
#'
#' Function to generate a given number of random steps for each observed step.
#'
#' @param x Steps.
#' @param n_control `[integer(1)=10]{>1}` \cr The number of control steps paired with each observed step.
#' @param sl_distr `[character(1)='gamma']{'gamma'}` \cr The distribution to be fitted to the empirical distribution of step lengths.
#' @param ta_distr `[character(1)='vonmises']{'vonmises', 'unif'}` \cr The distribution to be fitted to the empirical distribution of turn angles.
#' @param random.error `[numeric(1)=0.001]{>0}` \cr Upper limit for a uniformly distributed random error
#'   (between 0 and `random.error`) to be added to step lengths, to avoid step
#'   lengths of length 0.
#' @template dots_none
#' @export
#' @name random_steps
random_steps <- function(x, ...) {
  UseMethod("random_steps", x)
}

#' @export
#' @rdname random_steps
random_steps.steps <- function(x, n_control = 10, sl_distr = "gamma", ta_distr = "vonmises", random.error = 0.001,
                               ...) {
  if (any(is.na(x$sl_)) || any(is.na(x$ta_))) {
    x <- x[!is.na(x$sl_) & !is.na(x$ta_), ]
    warning("Step-lengths or turning angles contained NA, which were removed.")
  }

  # random.error
  if (random.error != 0) {
    x$sl_ <- x$sl_ + stats::runif(nrow(x), 0, random.error)
  }

  sl <- fit_sl_dist_base(x$sl_, distr = sl_distr)
  ta <- fit_ta_dist_base(x$ta_, distr = ta_distr)
  random_steps_base(x, n_control, sl, ta)
}

random_steps_base <- function(x, n_control, sl, ta) {
  # Generate random points
  ns <- nrow(x)  # number of steps
  case_for_control <- rep(1:ns, each = n_control)

  slr <-  if (sl$name %in% c("gamma", "exp", "weibull")) {
    do.call(paste0("r", sl$fit$distname), c(list(n = ns * n_control), as.list(sl$fit$estimate)))
  } else {
    stop("sl dist not implemented")
  }

  tar <- if (ta$name == "vonmises") {
    mu <- circular::as.circular(0, type = "angles", units = "degrees", template = "none",
                                modulo = "asis", zero = 0, rotation = "counter")
    # turn angles for new stps
    rta <- as.vector(circular::rvonmises(ns * n_control, mu = mu, kappa = ta$fit$kappa))
    rta <- (rta + x[case_for_control, ]$ta_) %% 360
    ifelse(rta > 180, rta - 360, rta)
  } else if (ta$name == "unif") {
    x[case_for_control, ]$ta_ + stats::runif(ns * n_control, -pi, pi)  # turning angles for new stps
  } else {
    stop("ta dist not implemented")
  }

  # control points
  xy_cc <- x[case_for_control, ]
  xy_cc["x2_"] <- xy_cc$x1_ + slr * cos(tar)
  xy_cc["y2_"] <- xy_cc$y2_ + slr * sin(tar)

  xy_cc$case_ <- FALSE
  xy_cc$step_id_ <- rep(1:ns, each = n_control)
  xy_cc$sl_ <- slr
  xy_cc$ta_ <- tar

  x$case_ <- TRUE
  x$step_id_ <- 1:ns
  has_burst <- "burst_" %in% names(x)

  vars <- c("step_id_", "case_", "x1_", "y1_", "x2_", "y2_", "t1_", "t2_", "dt_")
  if (has_burst) {
    vars <- c("burst_", vars)
  }

  # shuffle attributes in non_vars
  v1 <- base::setdiff(names(x), vars)
  #xy_cc <- xy_cc %>% mutate_at(v1, function(x) sample(unique(x), length(x), TRUE))
  #message("shuffling non standard columns")

  vars <- c(vars, v1)


  #out <- bind_rows(x, xy_cc) %>% arrange(quo("step_id_")) %>% select(.dots = vars)
  suppressWarnings(out <- bind_rows(x, xy_cc) %>% arrange(!!quo(step_id_)) %>% select(vars))

  class(out) <- c("random_steps", class(out))
  attributes(out)$sl_ <- sl
  attributes(out)$ta_ <- ta
  attributes(out)$n_control_ <- n_control
  out
}

rsteps_transfer_attr <- function(from, to) {
  from <- attributes(from)
  attributes(to)$class <- from$class
  attributes(to)$sl_ <- from$sl_
  attributes(to)$ta_ <- from$ta_
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
