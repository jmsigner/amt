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

#' @export
make_start <- function(x, ...) {
  UseMethod("make_start")
}

#' @export
make_start.numeric <- function(
  x = c(0, 0),
  ta_ = 0,
  time = Sys.time(), dt = hours(1), ...) {

  cc <- x
  out <- tibble::tibble(
    x_ = cc[1], y_ = cc[2], ta_ = ta_,
    t_ = time, dt = dt)
  class(out) <- c("sim_start", class(out))
  out
}

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
  out
}

wrap_angle <- function(x) {
  x <- x %% (2*pi)
  ifelse(x > pi, x - (2*pi), x)
}

get_max_dist <- function(x, ...) {
  UseMethod("get_max_dist")
}

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

# Old documentation
# #' @param object Model object
# #' @param initial.step First step
# #' @param start.t Start time
# #' @param delta_t Step duration
# #' @param map Environmental layers
# #' @param fun Function to build environmental predictors
# # #' @param n_steps How many steps to simulate
# #' @param n.control How many alternative steps are considered each step
# #' @param sl_model Step length model to use
# #' @param ta_model Turning angle model to use
# #' @return Simulated trajectory
# #' @export
# #'

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
  class(s1) <- c("steps_xy", "data.frame")
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
    w <- w + phi - log(xy$sl_)
  }
  w <- exp(w - mean(w[is.finite(w)], na.rm = TRUE))
  w[!is.finite(w)] <- 0
  w
}


kernel_setup <- function(template, max.dist = 100, start) {

  checkmate::assert_class(template, "RasterStack")
  checkmate::assert_number(max.dist, lower = 0)
  checkmate::assert_class(start, "sim_start")

  p <- sf::st_sf(
    geom = sf::st_sfc(sf::st_point(as.numeric(start[, c("x_", "y_")])))) %>%
    sf::st_buffer(dist = max.dist)

  # 2. Rasterize buffer
  r1 <- raster::rasterize(p, raster::crop(template, p))

  # 3. Get xy from buffer
  xy <- raster::rasterToPoints(r1)

  k <- tibble(
    x = xy[, 1],
    y = xy[, 2])
  k$ta_ = base::atan2(k$y - start$y_[1], k$x - start$x_[1]) - pi/2
  k$ta_ <- k$ta_ - start$ta_[1] - pi/2

  k$sl_ = sqrt((k$x - start$x_[1])^2 + (k$y - start$y_[1])^2)

  k <- data.frame(k, "x1_" = start$x_[1], "y1_" = start$y_[1]) %>%
    dplyr::rename(x2_ = x, y2_ = y)
  class(k) <- c("steps_xy", "data.frame")
  k
}

#' @export

redistribution_kernel <- function(
  x = make_issf_model(), start = make_start(),
  map,
  fun = function(xy, map) {
    extract_covariates(xy, map, where = "both")
  },
  max.dist = get_max_dist(x),
  n.control = 1e6,
  n.sample = 1,
  stochastic = FALSE,
  normalize = TRUE,
  interpolate = FALSE,
  as.raster = TRUE,
  tolerance.outside = 0) {

  arguments <- as.list(environment())
  checkmate::assert_class(start, "sim_start")

  if (FALSE) {
    x = k2$args$x
    start = start
    map = x$args$map
    fun = x$args$fun
    max.dist = x$args$max.dist
    n.control = x$args$n.control
    n.sample = 1
    stochastic = x$args$stochastic
    normalize = TRUE
    interpolate = FALSE
    as.raster = FALSE
    tolerance.outside = x$args$tolerance.outside
  }



  if (stochastic) {
    xy <- random_steps_simple(start, sl_model = x$sl_,
                              ta_model = x$ta_, n.control = n.control)
  } else {
    xy <- kernel_setup(map, max.dist, start)
  }

  # Check for the fraction of steps that is outside the landscape
  bb.map <- raster::bbox(map)
  fraction.outside <- mean(xy$x2_ < bb.map[1, 1] | xy$x2_ > bb.map[1, 2] |
                             xy$y2_ < bb.map[2, 1] | xy$y2_ > bb.map[2, 2])
  if (fraction.outside > tolerance.outside) {
    warning(paste0(round(fraction.outside * 100, 2),
                   "% of steps are ending outside the study area but only ",
                   round(tolerance.outside * 100, 2),
                   "% is allowed. ",
                   "Terminating simulations here."))
    return(NA) # Make sure something meaningful is returned
  }

  # Add time stamp
  xy$t_ <- start$t_

  # Extract covariate values
  xy <- fun(xy, map)

  w <- ssf_weights(xy, x, compensate.movement = !stochastic)

  ## Should we also provide an option for returning just a single step?
  r <- if (!as.raster) {
    xy[sample.int(nrow(xy), size = n.sample, prob = w), ] %>%
      dplyr::select(x_ = x2_, y_ = y2_, t_)
  } else {
    if (stochastic) {
      rasterize(
        xy[, c("x2_", "y2_")], w,
        as.numeric(start$x_[1], start$y_[1]), map, max.dist, interpolate)
    } else {
      raster::rasterFromXYZ(data.frame(xy[, c("x2_", "y2_")], w))
    }
  }

  if (as.raster & normalize) {
    r <- normalize(r)
  }

  res <- list(
    args = arguments,
    redistribution.kernel = r
  ) # We could add the timing here and later guess how long the simulation will take
  class(res) <- c("redistribution_kernel", "list")
  res
}

rasterize <- function(xy, w, position, map, max.dist, interpolate = FALSE) {
  w <- tibble::tibble(
    x = xy$x2_,
    y = xy$y2_,
    w = w
  )
  weights <- sf::st_as_sf(w, coords = c("x", "y"))
  p <- sf::st_sf(geom = sf::st_sfc(sf::st_point(position))) %>%
    sf::st_buffer(dist = max.dist)
  rr <- raster::crop(raster(map, 1), p)
  rr <- raster::rasterize(p, rr)
  r1 <- raster::rasterize(weights, rr, field = "w", fun = sum, background = 0)
  r1 <- raster::mask(r1, rr)
  r1
}

normalize <- function(x) {
  x / sum(raster::getValues(x), na.rm = TRUE)
  #  raster::setValues(x, raster::getValues(x) /
  #                      sum(raster::getValues(x), na.rm = TRUE))
}


movement_kernel1 <- function(x, sl.model, ta.model) {
  phi <- switch(
    sl.model$name,
    gamma = -x$sl_ / sl.model$params$scale + log(x$sl_) * (sl.model$params$shape - 1),
    exp = -x$sl_ * sl.model$params$rate)
  if(ta.model$name == "vonmises") {
    phi <- phi + cos(x$ta_) * ta.model$params$kappa
  }
  phi
}

#' @export
simulate_path <- function(x, ...) {
  UseMethod("simulate_path")
}

#' @export
simulate_path.default <- function(x, ...) {
  message("Please pass a redistribution kernel.")
}

#' @export
simulate_path.redistribution_kernel <- function(
  x, n.steps = 100, start = x$args$start, verbose = FALSE, ...) {


  if (FALSE) {
    x = k2
    n.steps = 100
    start = x$args$start
  }

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
      stochastic = mod$stochastic,
      normalize = TRUE,
      interpolate = FALSE,
      as.raster = FALSE,
      tolerance.outside = mod$tolerance.outside
    )$redistribution.kernel

    # Check that we do not have error (i.e., because stepping outside the landscape)

    # Make new start
    new.ta <- atan2(rk$y_[1] - start$y_[1], rk$x_[1] - start$x_[1])

    xy$x_[i] <- rk$x_[1]
    xy$y_[i] <- rk$y_[1]
    start <- make_start(as.numeric(xy[i, c("x_", "y_")]), new.ta)
  }
  return(xy)
}
