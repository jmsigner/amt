simulate <- function(
  object, start_xy, start_t, dt, map,
  fun = function(xy, map) {
    amt:::extract_covariates.steps_xy(xy, map)
  },
  n_steps = 1000,
  n_control = 200,
  angle_start = 0,
  sl_model = object$sl_, ta_model = object$ta_,
  n_random_draws = 1e5,
  verbose = FALSE) {

  ##
  if (FALSE) { # debug
    object <- xx
    fun = function(x, map, t) {
      x %>% extract_covariates(map) %>%
        time_of_day() %>% mutate(log_sl = log(sl_))
    }
    start_t = deer$t_[1]
    dt = hours(6)
    map = sh_forest
    start_xy = unlist(deer[1, c("x_", "y_")])
    n_steps = 1000
    n_control = 200
    sl_model = object$sl_
    ta_model = object$ta_
    n_random_draws = 1e5
    verbose = FALSE
    angle_start <- 0
    i <- 1
  }
  ##

  slr <- random_numbers(sl_model, n_random_draws)
  tar <- random_numbers(ta_model, n_random_draws)


  coefs <- coef(object)
  res <- matrix(0, ncol = 4, nrow = n_steps)
  colnames(res) <- c("x1_", "y1_", "x2_", "y2_")
  time_stamps <- seq(start_t, by = lubridate::as.difftime(dt),
                     len = n_steps)
  res[1, c("x1_", "y1_")] <- start_xy

  rhs <- strsplit(as.character(object$model$formula)[3], "\\+")[[1]]
  rhs <- rhs[-grep("strata", rhs)]
  ff <- as.formula(paste("~", paste(rhs, collapse = "+")))

  rel_angle <- angle_start

  for (i in 1:n_steps) {
    if(verbose) cat(i, "\n")
    ok <- FALSE
    ctr <- 1
    while (!ok && ctr < 5) {
      xy_try <- random_steps.numeric(
        start = res[i, c("x1_", "y1_")],
        n_control = ceiling(n_control * 1.2),
        rel_angle = rel_angle,
        rand_sl = slr,
        rand_ta = tar)

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
      if (NROW(xy) >= n_control) {
        ok <- TRUE
        xy <- xy[1:n_control,]
      }
      ctr <- ctr + 1
    }

    xyz <- stats::model.matrix.default(
      terms(ff, keep.order = TRUE), data = xy, na.action = na.pass)

    if (verbose) {
      cat("Formula: ", as.character(ff), "\n")
      cat("Names of coefficients: ", names(coefs), "\n")
      cat("Names of model matrix: ", colnames(xyz), "\n")
    }

    if (FALSE) { # possible fix
      cn <- names(coefs)
      mmn <- colnames(xyz)
      prob <- strsplit(cn[!cn %in% mmn], ":")[[1]]
      pp <- grepl(prob[1], mmn, fixed = TRUE) & grepl(prob[2], mmn, fixed = TRUE)
      mmn[pp] <-  paste0(rev(strsplit(mmn[pp], ":")[[1]]), collapse = ":")
      colnames(xyz) <- mmn
    }

    w <- as.matrix(xyz[, names(coefs)]) %*% coefs
    w <- exp(w - max(w, na.rm = TRUE))

    if (any(is.infinite(w[, 1]))) {
      warning("Infinite probabilities, consider rescaling variables.")
    }

    w[is.na(w)] <- 0

    selected <- try(sample.int(length(w), 1, prob = w))
    if (inherits(selected, "try-error")) {
      return(list(xyz, xy, res))
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
  }

  res <- data.frame(res)
  res$t1_ <- time_stamps
  res$t2_ <- res$t1_ + dt
  res$dt_ <- lubridate::as.difftime(dt)
  res
}
