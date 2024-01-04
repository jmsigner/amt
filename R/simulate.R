#' Create an `issf`-model object from scratch
#'
#' In order to simulate from an `issf` a
#'
#' @param coefs A named vector with the coefficient values.
#' @param sl The tentative step-length distribution.
#' @param ta The tentative turn-angle distribution.
#'
#' @return An object of `fit_clogit`.
#' @export
#'
make_issf_model <- function(
  coefs = c("sl_" = 0), sl = make_exp_distr(), ta = make_unif_distr())
{
  checkmate::assert_numeric(coefs, finite = TRUE)
  checkmate::assert_named(coefs)
  checkmate::assert_class(sl, "sl_distr")
  checkmate::assert_class(ta, "ta_distr")

  rhs <- c(names(coefs), "strata")
  structure(list(
    coefficients = coefs, sl_ = sl, ta_ = ta,
    model = list(formula =
                   stats::as.formula(paste("Surv ~ ", paste(rhs, collapse = "+"))))
  ),
  class = c("fit_clogit", "list")
  )
}

#' Create an initial step for simulations
#'
#' An initial step for simulations. This step can either be created by defining a step from
#' scratch or by using an observed step.
#'
#' @param x `[steps_xyt,numeric(2)]` \cr A step of class `steps_xyt` or the start coordinates..
#' @param ta_ `[numeric(1)]{0}` \cr The initial turn-angle.
#' @param time `[POSIXt(1)]{Sys.time()}` \cr The time stamp when the simulation
#'   starts.
#' @param dt `[Period(1)]{hours(1)}` \cr The sampling rate of the
#'   simulations.
#' @param crs `[int(1)]{NA}` \cr The coordinate reference system of the start location given as EPSG code.
#' @template dots_none
#' @name make_start
#'
#' @export
make_start <- function(x, ...) {
  UseMethod("make_start")
}

#' @rdname make_start
#' @export
make_start.numeric <- function(
  x = c(0, 0),
  ta_ = 0,
  time = Sys.time(), dt = hours(1),
  crs = NA, ...) {

  cc <- x
  out <- tibble::tibble(
    x_ = cc[1], y_ = cc[2], ta_ = ta_,
    t_ = time, dt = dt)
  class(out) <- c("sim_start", class(out))
  attr(out, "crs") <- crs
  out
}

#' @rdname make_start
#' @export
make_start.track_xyt <- function(x, ta_ = 0, dt = hours(1), ...) {
  if (nrow(x) > 1) {
    warning("More than one point provided, only the first will be used as a starting step")
    x <- x[1, ]
  }
  out <- tibble::tibble(
    x_ = x$x_[1], y_ = x$y_[1], ta_ = ta_,
    t_ = x$t_[1], dt = dt)
  class(out) <- c("sim_start", class(out))
  attr(out, "crs") <- get_crs(x)
  out
}

#' @rdname make_start
#' @export
make_start.steps_xyt <- function(x, ...) {
  if (nrow(x) > 1) {
    warning("More than one step provided, only the first will be used as a starting step")
    x <- x[1, ]
  }
  out <- tibble::tibble(
    x_ = x$x1_[1], y_ = x$y1_[1], ta_ = x$ta_[1],
    t_ = x$t1_[1], dt = x$dt_[1])
  class(out) <- c("sim_start", class(out))
  attr(out, "crs") <- get_crs(x)
  out
}

wrap_angle <- function(x) {
  x <- x %% (2*pi)
  ifelse(x > pi, x - (2*pi), x)
}

#' Get the maximum distance
#'
#' Helper function to get the maximum distance from a fitted model.
#'
#' @export
#' @name get_max_dist

get_max_dist <- function(x, ...) {
  UseMethod("get_max_dist")
}

#' @param x `[fitted_issf]` \cr A fitted integrated step-selection function.
#' @param p `[numeric(1)]{0.99}` The quantile of the step-length distribution.
#' @template dots_none
#' @export
#' @rdname get_max_dist
get_max_dist.fit_clogit <- function(x, p = 0.99, ...) {
  checkmate::assert_number(p, lower = 0, upper = 1)
  ceiling(do.call(paste0("q", x$sl_$name), c(list("p" = p), x$sl_$params)))
}


#' Simulate from an ssf model
#'
#' @param start First step
#' @param n.control How many alternative steps are considered each step
#' @param sl_model Step length model to use
#' @param ta_model Turning angle model to use
#' @return Simulated trajectory
#' @export

random_steps_simple <- function(start, sl_model, ta_model, n.control) {

  checkmate::assert_class(sl_model, "sl_distr")
  checkmate::assert_class(ta_model, "ta_distr")
  checkmate::assert_number(n.control, lower = 1)

  slr <- random_numbers(sl_model, n = n.control)
  tar <- random_numbers(ta_model, n = n.control)


  new_x <- start$x_[1] + slr * cos(start$ta_[1] + tar)
  new_y <- start$y_[1] + slr * sin(start$ta_[1] + tar)

  s1 <- data.frame(
    "x1_" = unname(start$x_[1]),
    "y1_" = unname(start$y_[1]),
    "x2_" = new_x,
    "y2_" = new_y,
    "sl_" = slr,
    "ta_" = tar)
  attr(s1, "crs") <- attr(start, "crs")
  class(s1) <- c("steps_xyt", "steps_xy", "data.frame")
  s1
}


#' Takes a `clogit` formula and returns a formula without the `strata` and the
#' left-hand side
#' @param formula A formula object
#' @export
#' @examples
#' f1 <- case_ ~ x1 * x2 + strata(step_id_)
#' ssf_formula(f1)

ssf_formula <- function(formula) {
  rhs <- strsplit(as.character(formula)[3], "\\+")[[1]]
  rhs <- rhs[-grep("strata", rhs)]
  stats::as.formula(paste("~", paste(rhs, collapse = "+")))
}

#' Given a fitted ssf, and new location the weights for each location is
#' calculated
#'
#' @param xy The new locations.
#' @param object The the fitted (i)SSF.
#' @param compensate.movement Whether or not for the transformation from polar
#'   to Cartesian coordinates is corrected.
ssf_weights <- function(xy, object, compensate.movement = FALSE) {

  checkmate::assert_class(xy, "data.frame")
  checkmate::assert_class(object, "fit_clogit")
  checkmate::assert_logical(compensate.movement)

  coefs <- coef(object)
  ff <- ssf_formula(object$model$formula)
  newdata <- xy
  attr(newdata, "na.action") <- "na.pass"
  xyz <- stats::model.matrix.default(ff, data = newdata, na.action = stats::na.pass)
  w <- as.matrix(xyz[, names(coefs)]) %*% coefs

  if (compensate.movement) {
     phi <- movement_kernel1(xy, object$sl_, object$ta_)
     w <- w + phi - log(xy$sl_) # -log(xy$sl) divides by the sl and accounts for the transformation
  }
  w <- exp(w - mean(w[is.finite(w)], na.rm = TRUE))
  w[!is.finite(w)] <- 0
  w
}


kernel_setup <- function(template, max.dist = 100, start, covars) {

  checkmate::assert_class(template, "SpatRaster")
  checkmate::assert_number(max.dist, lower = 0)
  checkmate::assert_class(start, "sim_start")

  p <- sf::st_sf(
    geom = sf::st_sfc(sf::st_point(as.numeric(start[, c("x_", "y_")])))) |>
    sf::st_buffer(dist = max.dist)

  # 2. Rasterize buffer
  r1 <- terra::rasterize(terra::vect(p), terra::crop(template, p))

  # 3. Get xy from buffer
  xy <- terra::crds(r1)

  k <- tibble(
    x = xy[, 1],
    y = xy[, 2])
  k$ta_ = base::atan2(k$y - start$y_[1], k$x - start$x_[1])

  # Adjust angles
  k$ta_ <- ifelse(k$ta_ < 0, 2 * pi + k$ta_, k$ta_) # To full circle
  k$ta_ <- (k$ta_ - start$ta_[1]) %% (2 * pi) # for start direction
  k$ta_ <- ifelse(k$ta_ > pi, (2 * pi - k$ta_) * -1, k$ta_) # only use half circle

  k$sl_ = sqrt((k$x - start$x_[1])^2 + (k$y - start$y_[1])^2)

  k <- data.frame(k, "x1_" = start$x_[1], "y1_" = start$y_[1]) |>
    dplyr::rename(x2_ = x, y2_ = y)

  if (!is.null(covars)) {
    checkmate::assert_tibble(covars, max.rows = 1)
    k <- dplyr::bind_cols(k, covars, .name_repair = "check_unique")
  }

  class(k) <- c("steps_xyt", "steps_xy", class(k))
  attr(k, "crs") <- attr(start, "crs")
  k
}

#' Create a redistribution kernel
#'
#' From a fitted integrated step-selection function for a given position a
#' redistribution kernel is calculated (i.e., the product of the movement kernel
#' and the selection function).
#
#' @param x `[fit_issf]` \cr A fitted integrated step-selection function. Generated either with `fit_issf()` or make `make_issf_model()`.
#' @param start `[sim_start]` \cr The start position in space and time. See `make_start()`.
#' @param map `[SpatRaster]` \cr A SpatRaster with all covariates.
#' @param fun `[function]` \cr A function that is executed on each location of the redistribution kernel. The default function is `extract_covariates()`.
#' @param max.dist `[numeric(1)]` \cr The maximum distance of the redistribution kernel.
#' @param n.control `[integer(1)]{1e6}` \cr The number of points of the redistribution kernel (this is only important if `landscape = "continuous"`).
#' @param n.sample `[integer(1)]{1}` \cr The number of points sampled from the redistribution kernel (this is only important if `as.rast = FALSE`).
#' @param landscape `[character(1)]{"continuous"}` \cr If `"continuous` the redistribution kernel is sampled using a random sample of size `n.control`. If `landscape = "discrete"` each cell in the redistribution kernel is used.
#' @param normalize `[logical(1)]{TRUE}` \cr If `TRUE` the redistribution kernel is normalized to sum to one.
#' @param interpolate `[logical(1)]{FALSE}` \cr If `TRUE` a stochastic redistribution kernel is interpolated to return a raster layer. Note, this is just for completeness and is computationally inefficient in most situations.
#' @param as.rast `[logical(1)]{TRUE}` \cr If `TRUE` a `SpatRaster` should be returned.
#' @param tolerance.outside `[numeric(1)]{0}` \cr The proportion of the redistribution kernel that is allowed to be outside the `map`.
#' @param covars `[tibble]` \cr Additional covariates that might be used in the model (e.g., time of day).
#' @param compensate.movement `[logical(1)]` \cr Indicates if movement parameters are corrected or not. This only relevant if `stochastic = FALSE`.
#'
#' @export

redistribution_kernel <- function(
  x = make_issf_model(),
  start = make_start(),
  map,
  fun = function(xy, map) {
    extract_covariates(xy, map, where = "both")
  },
  covars = NULL,
  max.dist = get_max_dist(x),
  n.control = 1e6,
  n.sample = 1,
  stochastic = NULL,
  landscape = "continuous",
  compensate.movement = landscape == "discrete",
  normalize = TRUE,
  interpolate = FALSE,
  as.rast = FALSE,
  tolerance.outside = 0) {

  arguments <- as.list(environment())
  checkmate::assert_class(start, "sim_start")

  if (!missing("stochastic")) {
    warning("Argument stochastic is deprecated, use the argument 'landscape' instead.")
  }

  if (!landscape %in% c("continuous", "discrete")) {
    stop("Argument `landscape` is invalid. Valid values are 'continuous' or 'discrete'.")
  }


  if (landscape == "continuous") {
    xy <- random_steps_simple(start, sl_model = x$sl_,
                              ta_model = x$ta_, n.control = n.control)
  } else {
    xy <- kernel_setup(map, max.dist, start, covars)
  }

  # Check for the fraction of steps that is outside the landscape
  bb.map <- as.vector(terra::ext(map))
  fraction.outside <- mean(
    xy$x2_ < bb.map["xmin"] | xy$x2_ > bb.map["xmax"] |
      xy$y2_ < bb.map["ymin"] | xy$y2_ > bb.map["ymax"]
  )
  if (fraction.outside > tolerance.outside) {
    warning(paste0(round(fraction.outside * 100, 3),
                   "% of steps are ending outside the study area but only ",
                   round(tolerance.outside * 100, 3),
                   "% is allowed. ",
                   "Terminating simulations here."))
    return(NULL) # Make sure something meaningful is returned
  }

  # Add time stamp
  xy$t1_ <- start$t_
  xy$t2_ <- start$t_ + start$dt

  # Extract covariate values
  xy <- fun(xy, map)

  w <- ssf_weights(xy, x, compensate.movement = compensate.movement)

  r <- if (!as.rast) {
    xy[sample.int(nrow(xy), size = n.sample, prob = w), ] |>
      dplyr::select(x_ = x2_, y_ = y2_, t2_)
  } else {
    if (landscape == "continuous") {
      stop("`as.rast` not implemented for `landscape = 'continuous'`")
    } else {
      terra::rast(data.frame(xy[, c("x2_", "y2_")], w))
    }
  }

  if (as.rast & normalize) {
    r <- normalize(r)
  }

  res <- list(
    args = arguments,
    redistribution.kernel = r
  ) # We could add the timing here and later guess how long the simulation will take
  class(res) <- c("redistribution_kernel", "list")
  res
}

# rasterize <- function(xy, w, position, map, max.dist, interpolate = FALSE) {
#   w <- tibble::tibble(
#     x = xy$x2_,
#     y = xy$y2_,
#     w = w
#   )
#   weights <- sf::st_as_sf(w, coords = c("x", "y"))
#   p <- sf::st_sf(geom = sf::st_sfc(sf::st_point(position))) |>
#     sf::st_buffer(dist = max.dist)
#   rr <- raster::crop(raster(map, 1), p)
#   rr <- raster::rasterize(p, rr)
#   r1 <- raster::rasterize(weights, rr, field = "w", fun = sum, background = 0)
#   r1 <- raster::mask(r1, rr)
#   r1
# }

normalize <- function(x) {
  x / sum(x[], na.rm = TRUE)
}


movement_kernel1 <- function(x, sl.model, ta.model) {
  phi <- switch(
    sl.model$name,
    # gamma = -x$sl_ / sl.model$params$scale + log(x$sl_) * (sl.model$params$shape - 1),
    gamma = -1 / sl.model$params$scale * x$sl_ + # this is the adjustment term for scale
      log(x$sl_) * (sl.model$params$shape - 1),
    exp = -x$sl_ * sl.model$params$rate,
    hnorm = x$sl
  )
  if(ta.model$name == "vonmises") {
    phi <- phi + cos(x$ta_) * ta.model$params$kappa
  }
  phi
}

#' Simulate a movement trajectory.
#'
#' Function to simulate a movement trajectory (path) from a redistribution kernel.
#' @param x `[redstirubtion_kernel(1)]` \cr An object of class `redistribution_kernel`.
#' @template dots_none
#' @name simulate_path
#'
#'
#' @export
simulate_path <- function(x, ...) {
  UseMethod("simulate_path")
}

#' @export
#' @rdname simulate_path
simulate_path.default <- function(x, ...) {
  message("Please pass a redistribution kernel.")
}

#' @param n.steps `[integer(1)]{100}` \cr The number of simulation steps.
#' @param start `[sim_start]` \cr The starting point in time and space for the simulations (see `make_start()`).
#' @param verbose `[logical(1)]{FALSE}` If `TRUE` progress of simulations is displayed.
#' @export
#' @rdname simulate_path
simulate_path.redistribution_kernel <- function(
  x, n.steps = 100, start = x$args$start, verbose = FALSE, ...) {

  mod <- x$args
  xy <- tibble(x_ = rep(NA, n.steps + 1), y_ = NA_real_,
               t_ = start$t_ + start$dt * (0:n.steps), dt = start$dt)

  xy$x_[1] <- start$x_
  xy$y_[1] <- start$y_


  for (i in 1:n.steps) {
    rk <- redistribution_kernel(
      x = mod$x,
      start = start,
      map = mod$map,
      fun = mod$fun,
      max.dist = mod$max.dist,
      n.control = mod$n.control,
      n.sample = 1,
      landscape = mod$landscape,
      normalize = TRUE,
      interpolate = FALSE,
      as.rast = FALSE,
      tolerance.outside = mod$tolerance.outside
    )

    if (is.null(rk)) {
      warning(paste0("Simulation stopped after ", i - 1, " time steps, because the animal stepped out of the landscape."))
      return(xy)
    }

    rk <- rk$redistribution.kernel

    # Check that we do not have error (i.e., because stepping outside the landscape)
    # Make new start
    new.ta <- atan2(rk$y_[1] - start$y_[1], rk$x_[1] - start$x_[1])

    xy$x_[i + 1] <- rk$x_[1]
    xy$y_[i + 1] <- rk$y_[1]
    start <- make_start(
      as.numeric(xy[i + 1, c("x_", "y_")]), new.ta,
      time = xy$t_[i],
      crs = attr(x$args$start, "crs"))
  }
  return(xy)
}
