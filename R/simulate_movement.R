# https://stackoverflow.com/questions/43717794/best-practice-for-defining-s3-methods-with-different-arguments



adjust_distribution <- function(x, ...) {
  UseMethod("adjust_distribution")
}

adjust_distribution.amt_distr <- function(x, ...) {
  x
}

adjust_distribution.adjustable_exp_distr <- function(x, covars) {
  if (!is.null(x$rate)) {
    # change rate
    tentative_rate <- x$dist$params$rate

    # mod sl
    modifiers <- attr(x$rate, "rhs")
    mod1_coef <- trimws(strsplit(deparse(modifiers[[1]]), "\\+")[[1]])
    mod2_coef <- trimws(strsplit(deparse(modifiers[[2]]), "\\+")[[1]])
    mod2_covar <- gsub(paste0(mod1_coef, ":"), "",  mod2_coef)

    # check names match
    if (!all(mod2_covar %in% names(covars))) {
      stop("Some coefficients not found in covariates")
    }

    covars <- unlist(covars[1, ], use.names = TRUE)
    new_rate <- tentative_rate +
      x$coefs[mod1_coef] + sum(x$coefs[mod2_coef] * covars[mod2_covar])
    make_exp_distr(rate = new_rate)
  } else {
    x$dist
  }
}

adjust_distribution.vonmises_distr <- function(x, covars) {
  if (!is.null(x$kappa)) {
    # change rate
    tentative_kappa <- x$dist$params$kappa

    # mod sl
    modifiers <- attr(x$kappa, "rhs")
    mod1_coef <- trimws(strsplit(deparse(modifiers[[1]]), "\\+")[[1]])
    mod2_coef <- trimws(strsplit(deparse(modifiers[[2]]), "\\+")[[1]])
    mod2_covar <- gsub(paste0(mod1_coef, ":"), "",  mod2_coef)

    # check names match
    if (!all(mod2_covar %in% names(covars))) {
      stop("Some coefficients not found in covariates")
    }

    covars <- unlist(covars[1, ], use.names = TRUE)
    new_kappa <- tentative_kappa +
      x$coefs[mod1_coef] + sum(x$coefs[mod2_coef] * covars[mod2_covar])
    make_vonmises_distr(kappa = new_kappa)
  } else {
    x$dist
  }
}

make_habitat_kernel <- function(f, coefs) {
  checkmate::assert_vector(coefs, names = "named")
  f <- check_formula(f, coefs)
  x <- list(f = f, coefs = coefs[attr(terms(f), "term.labels")])
  class(x) <-  c("amt_habitat_kernel", class(x))
  x
}

make_movement_kernel <- function(step_length_dist, turn_angle_dist) {
  checkmate::assert_class(step_length_dist, "amt_distr")
  checkmate::assert_class(turn_angle_dist, "amt_distr")
  x <- list(step_length_dist = step_length_dist,
            turn_angle_dist = turn_angle_dist)
  class(x) <-  c("amt_movement_kernel", class(x))
  x
}


simulate_movement <- function(
  habitat_kernel,
  movement_kernel,
  map,
  fun = function(x, map) {
    extract_covariates(x, map)
  },
  start_xy = c(0, 0),
  start_angle = runif(1, -pi, pi),
  start_t = now(),
  dt = hours(3),
  n_steps = 1000,
  n_choices = 100,
  verbose = FALSE,
  edge = "bounce"
) {

  # check input
  res <- tibble::tibble(x1_ = rep(NA, n_steps), y1_ = NA, x2_ = NA, y2_ = NA,
                        sl_ = NA, ta_ = NA)
  res$t1_ <- time_stamps <- seq(start_t, by = lubridate::as.difftime(dt), len = n_steps)
  res$t2_ <- res$t1_ + dt
  res[1, c("x1_", "y1_")] <- start_xy
  class(res) <- c("steps_xyt", "steps_xy", "tbl_df", "tbl", "data.frame")

  rel_angle <- start_angle

  if (edge == "torus") {
    xmx <- raster::xmax(map)
    xmn <- raster::xmin(map)
    ymx <- raster::ymax(map)
    ymn <- raster::ymin(map)
  }

  for (i in 1:n_steps) {
    if(verbose) cat(i, "\n")
    ok <- FALSE
    ctr <- 1

    sld <- if (is(movement_kernel$step_length_dist, "adjustable_distr")) {
      adjust_distribution(movement_kernel$step_length_dist, fun(res[i, ], map))
    } else {
      movement_kernel$step_length_dist
    }

    tad <- if (is(movement_kernel$turn_angle_dist, "adjustable_distr")) {
      adjust_distribution(movement_kernel$turn_angle_dist, fun(res[i, ], map))
    } else {
      movement_kernel$turn_angle_dist
    }

    if (edge == "torus") {
      xy1 <- random_steps.numeric(
        start = unlist(res[i, c("x1_", "y1_")], use.names = FALSE),
        n_control = ceiling(n_choices * 1.2),
        rel_angle = rel_angle,
        rand_sl = random_numbers(sld),
        rand_ta = random_numbers(tad))

      # not yet optimal
      xy <- as.data.frame(xy1)
      attr(xy, "crs_") <- sp::CRS(raster::projection(map))
      class(xy) <- c("steps_xyt", "steps_xy", "data.frame")

      # make sure animal stays within the boundaries
      xy$x2_ <- ifelse(xy$x2_ > xmx, (xy$x2_ %% xmx) + xmn, xy$x2_)
      xy$y2_ <- ifelse(xy$y2_ > ymx, (xy$y2_ %% xmx) + ymn, xy$y2_)

      xy$t1_ <- time_stamps[i]
      xy$t2_ <- time_stamps[i] + dt
      xy2 <- fun(xy, map)
      xy <- na.omit(xy2)

    } else if (edge == "bounce") {
      while (!ok && ctr < 5) {
        xy_try <- random_steps.numeric(
          start = unlist(res[i, c("x1_", "y1_")], use.names = FALSE),
          n_control = ceiling(n_choices * 1.2),
          rel_angle = rel_angle,
          rand_sl = random_numbers(sld),
          rand_ta = random_numbers(tad))

        # not yet optimal
        xy_try <- as.data.frame(xy_try)
        attr(xy_try, "crs_") <- sp::CRS(raster::projection(map))
        class(xy_try) <- c("steps_xyt", "steps_xy", "data.frame")

        xy_try$t1_ <- time_stamps[i]
        xy_try$t2_ <- time_stamps[i] + dt
        xy_try <- fun(xy_try, map)
        xy_try <- na.omit(xy_try)

        if (ctr == 1) {
          xy <- xy_try
        } else {
          xy <- rbind(xy, xy_try)
        }
        if (NROW(xy) >= n_choices) {
          ok <- TRUE
          xy <- xy[1:n_choices,]
        } else if (NROW(xy) < floor(n_choices / 10) ){
          warning("Simulation terminated early, not enough choices.")
          return(list(attempts = xy, res = res))
        }
        ctr <- ctr + 1
      }
    }
    xyz <- stats::model.matrix(
      terms(habitat_kernel$f, keep.order = TRUE), data = xy, na.action = na.pass)

    w <- as.matrix(xyz[, names(habitat_kernel$coefs)]) %*% habitat_kernel$coefs
    w <- try(exp(w - max(w, na.rm = TRUE)))

    if (inherits(w, "try-error")) {
      return(list(i = i, w = w, xyz = xyz, xy = xy, res = res))
    }


    if (any(is.infinite(w[, 1]))) {
      warning("Infinite probabilities, consider rescaling variables.")
    }

    selected <- try(sample.int(length(w), 1, prob = w))

    if (inherits(selected, "try-error")) {
      return(list(i = i, w = w, xyz = xyz, xy = xy, res = res,
                  xy1 = xy1, xy2 = xy2, ts = time_stamps, dt = dt,
                  where = "after sampling"))
    }
    new_x <- xy[selected, "x2_"]
    new_y <- xy[selected, "y2_"]
    rel_angle <- xy$ta_[selected]

    if (i != n_steps) {
    res[i + 1 , c("x1_", "y1_")] <-
      res[i, c("x2_", "y2_")] <- c(new_x, new_y)
    } else {
      res[i, c("x2_", "y2_")] <- c(new_x, new_y)
    }
    res[i, c("sl_", "ta_")] <- xy[selected, c("sl_", "ta_")]
  }

  res$dt_ <- dt
  res
}
