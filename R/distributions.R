valid_ta_distr <- function(...) {
  c("vonmises", "unif")
}

valid_sl_distr <- function(...) {
  c("exp", "gamma", "unif")
}

valid_distr <- function(...) {
  c(valid_ta_distr(), valid_sl_distr())
}


valid_distr_params <- function(dist_name, params) {
  if (dist_name == "vonmises") {
    return(all(c("kappa", "mu") %in% names(params)))
  } else if (dist_name == "unif") {
    return(all(c("min", "max") %in% names(params)))
  } else if (dist_name == "exp") {
    return(c("rate") %in% names(params))
  } else if (dist_name == "gamma") {
    return(all(c("shape", "rate") %in% names(params)) |
      all(c("shape", "scale") %in% names(params)))
  }
  FALSE
}

#' Display availabel distributions for step lengths and turn angles.
#'
#' @param which_dist `[char(1)="all"]{"all", "ta", "sl"}` \cr Should `all`
#'   distributions be returned, or only distributions for turn angles (`ta`) or
#'   step lengths (`sl`).
#' @param names_only `[logical(1)=FALSE]` \cr Indicates if only the names of
#'   distributions should be returned.
#' @param ... none implemented.
#' @export
#'
available_distr <- function(which_dist = "all", names_only = FALSE, ...) {

  checkmate::check_character(which_dist, len = 1)

  ta <- tibble::tibble(what = "ta", dist = valid_ta_distr())
  sl <- tibble::tibble(what = "sl", dist = valid_sl_distr())
  if (which_dist == "ta") {
    ret <- ta
  } else if (which_dist == "sl") {
    ret <- sl
  } else if (which_dist == "all") {
    ret <- dplyr::bind_rows(ta, sl)
  }
  if (names_only) {
    dplyr::pull(ret, 2)
  } else {
    ret
  }
}


#' Functions to work with distributions as objects
#'
#' `make_distributions` creates a distribution.
#'
#' @param name `[char(1)]` \cr Short name of distribution. See `available_distr()`
#'   for all currently implemented distributions.
#' @param params `[list]` \cr A named list with parameters of the distribution.
#' @param ... none implemented.
#' @export
#' @name distributions

make_distribution <- function(name, params, ...) {
  checkmate::check_character(name, len = 1)
  checkmate::check_list(params)

  # check name
  if (!name %in% available_distr(names_only = TRUE)) {
    stop(glue::glue("{name} is not implemented."))
  }

  # check params
  if (!valid_distr_params(name, params)) {
    stop(glue::glue("Parameters for {name} are not valid."))
  }
  out <- list(name = name,
              params = params)

  class(out) <- c(paste0(name, "_distr"),
                  if (name %in% valid_ta_distr()) "ta_distr" else "sl_distr",
                  "amt_distr", "list")
  out
}

#' @export
#' @param rate `[double(1)>0]` \cr The rate of the exponential distribution.
#' @rdname distributions
make_exp_distr <- function(rate = 1) {
  checkmate::check_number(rate, lower = 0)
  make_distribution(name = "exp", params = list(rate = rate))
}

#' @export
#' @rdname distributions
#' @param min `[double(1)]` \cr The minimum of the uniform distribution.
#' @param max `[double(1)]` \cr The minimum of the uniform distribution.
make_unif_distr <- function(min = -pi, max = pi) {
  checkmate::check_number(min)
  checkmate::check_number(max)
  make_distribution(name = "unif", params = list(min = min, max = max))
}

#' @export
#' @rdname distributions
#' @param kappa `[double(1)>=0]` \cr Concentration parameter of the von Mises distribution.
make_vonmises_distr <- function(kappa = 1) {
  checkmate::check_number(kappa, lower = 0)
  make_distribution(name = "vonmises", params = list(kappa = kappa, mu = 0))
}

#' @export
#' @rdname distributions
#' @param shape,scale `[double(1)>=0]` \cr Shape and scale of the Gamma distribution
make_gamma_distr <- function(shape = 1, scale = 1) {
  checkmate::check_number(shape)
  checkmate::check_number(scale)
  make_distribution(name = "gamma", params = list(shape = shape, scale = scale))
}


# Random numbers ----------------------------------------------------------

#' @export
#' @rdname distributions
random_numbers <- function(x, n = 100, ...) {
  UseMethod("random_numbers")
}

#' @export
#' @param x `[amt_distr]` \cr A distribution object.
#' @param n `[integer(1)=100]{>0}` \cr The number of random draws.
#' @rdname distributions
random_numbers.vonmises_distr <- function(x, n = 100, ...) {

  # mu <- circular::as.circular(
  #   x$params$mu, type = "angles", units = "radians",  template = "none",
  #   modulo = "asis", zero = 0, rotation = "counter")

  suppressWarnings(
    x <- do.call(circular::rvonmises, c(list(n = n), x$params)))

  # turn angles for new stps
  x <- x %% (2 * pi)
  ifelse(x > base::pi, x - (2 * base::pi), x)
}

#' @export
#' @rdname distributions
random_numbers.amt_distr <- function(x, n = 100, ...) {
  do.call(paste0("r", x$name), c(list(n = n), x$params))
}



# Fit distr ---------------------------------------------------------------

#' Fit distribution to data
#'
#' Wrapper to fit a distribution to data. Currently implemented distributions
#' are the exponential distribution (`exp`), the gamma distribution (`gamma`)
#' and the von Mises distribution (`vonmises`).
#'
#' @param x `[numeric(>1)]` \cr The observed data.
#' @param dist_name `[character(1)]{"exp", "gamma", "unif", "vonmises"}` \cr The name of the
#'   distribution.
#' @param na.rm `[logical(1)=TRUE]` \cr Indicating whether `NA` should be
#'   removed before fitting the distribution.
#'
#' @return An `amt_distr` object, which consists of a list with the `name` of
#'   the distribution and its parameters (saved in `params`).
#' @export
#'
#' @examples
#' set.seed(123)
#' dat <- rexp(1e3, 2)
#' fit_distr(dat, "exp")
fit_distr <- function(x, dist_name, na.rm = TRUE) {

  checkmate::check_numeric(x)
  checkmate::check_character(dist_name, len = 1)
  if(!dist_name %in% valid_distr()) {
    stop("Distribution is currently not supported.")
  }

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  # TODO: also save SE?
  switch(dist_name,
    gamma = {
      fit <- fitdistrplus::fitdist(x, "gamma", keepdata = FALSE, lower = 0)
      make_gamma_distr(shape = fit$estimate["shape"], scale = 1 / fit$estimate["rate"])
    },
    exp = {
      fit <- fitdistrplus::fitdist(x, "exp", keepdata = FALSE)
      make_exp_distr(rate = fit$estimate["rate"])
    },
    vonmises = {
      xx <- circular::as.circular(
        x, type = "angles", units = "radians", template = "none",
        modulo = "asis", zero = 0, rotation = "counter")
      fit <- circular::mle.vonmises(xx)
      make_vonmises_distr(kappa = fit$kappa)
    }
  )
}


# Adjustable distributions ------------------------------------------------

#' Adjust distributional parameters

#' @export
#' @param x `[amt_distr]` \cr A distribution object.
#' @name adjustable_distributions

adjustable_distribution <- function(x, ...) {
  UseMethod("adjustable_distribution")
}

check_formula <- function(f, coefs, rm_intercept = FALSE) {
  checkmate::assert_formula(f, null.ok = TRUE)
  if (!is.null(f)) {
    f <- Formula::as.Formula(f)
    if (rm_intercept) {
      if(attr(terms(f), "intercept")) {
        f <- update(f, ~ . -1)
      }
    }
    if(!all(attr(terms(f), "term.labels") %in% names(coefs))) {
      stop("Some terms do not have a coefficient")
    }
  }
  f
}

#' @export
#' @param shape `[formula = NULL]` \cr A formula to adjust the shape parameter.
#' @param scale `[formula = NULL]` \cr A formula to adjust the scale parameter.
#' @param coefs `[names vector]` \cr Coefficients for the adjustment (names must match the formula).
#' @rdname adjustable_distributions
#'
adjustable_distribution.gamma_distr <- function(x, shape = NULL, scale = NULL, coefs) {
  checkmate::assert_vector(coefs, names = "named")
  scale <- check_formula(scale, coefs)
  shape <- check_formula(shape, coefs)
  xx <- list(dist = x, shape = shape, scale = scale,
             coefs = coefs)
  class(xx) <- c("adjustable_gamma_distr", "adjustable_distr", class(x))
}


#' @export
#' @param rate `[formula = NULL]` \cr A formula to adjust the rate parameter.
#' @rdname adjustable_distributions
adjustable_distribution.exp_distr <- function(x, rate = NULL, coefs) {
  checkmate::assert_vector(coefs, names = "named")
  rate <- check_formula(rate, coefs)
  xx <- list(dist = x, rate = rate, coefs = coefs)
  class(xx) <- c("adjustable_exp_distr", "adjustable_distr", class(x))
  xx
}

#' @export
#' @param kappa `[formula = NULL]` \cr A formula to adjust the kappa parameter.
#' @rdname adjustable_distributions
adjustable_distribution.vonmises_distr <- function(x, kappa = NULL, coefs) {
  checkmate::assert_vector(coefs, names = "named")
  kappa <- check_formula(kappa, coefs)
  xx <- list(dist = x, kappa = kappa, coefs = coefs)
  class(xx) <- c("adjustable_von_mises_distr", "adjustable_distr", class(x))
  xx
}





# # Adjust parameters -------------------------------------------------------
#
# #' Adjust distributional parameters
#
# #' @export
# #' @param x `[amt_distr]` \cr A distribution object.
# #' @param n `[integer(1)=100]{>0}` \cr The number of random draws.
# #' @rdname distributions
# adjust_distr <- function(x, ...) {
#   UseMethod("adjust_distr")
# }
#
# #' @export
# #' @rdname adjust_distr
# #'
# adjust_distr.gamma_distr <- function(
#   x, shape = NULL, scale = NULL,
#   coefs, model_data, ...
# ) {
#
#   # Adjust shape
#   new_shape <- if (!is.null(shape)) {
#     adj <- x$params$shape + adjust_base(shape, coefs, model_data)
#     if (adj < 0) {
#       warning("Adjusted shape is < 0, it was set to 0.")
#       adj <- 0
#     }
#     adj
#   } else {
#     x$params$shape
#   }
#
#   # Adjust scale
#   new_scale <- if (!is.null(scale)) {
#     modifier <- adjust_base(scale, coefs, model_data)
#     adj <- 1 / ((1 / x$params$scale) - modifier)
#     if (adj < 0) {
#       warning("Adjusted scale is < 0, it was set to 0.")
#       adj <- 0
#     }
#     adj
#   } else {
#     x$params$scale
#   }
#
#  make_gamma_distr(shape = new_shape, scale = new_scale)
#

#' @export
#' @rdname adjust_distr
#'

adjust_distr.vonmises_distr <- function(
  x, kappa = NULL,
  coefs, model_data, ...
) {
  # Adjust shape
  new_kappa <- if (!is.null(kappa)) {
    adj <- x$params$kappa + adjust_base(kappa, coefs, model_data)
    if (adj < 0) {
      warning("Adjusted kappa is < 0, it was set to 0.")
      adj <- 0
    }
    adj
  } else {
    x$params$shape
  }
  make_vonmises_distr(kappa = new_kappa)
}

adjust_base <- function(formula, coefs, model_data) {
  # check input classes
  checkmate::assert_formula(formula)
  checkmate::assert_numeric(coefs, names = "named")
  checkmate::assert_data_frame(model_data, nrows = 1)

  # check that there is only 1 first order term
  aterms <- terms(formula)
  tl <- attr(aterms, "term.labels")
  name_ <- tl[which(attr(terms(formula), "order") == 1)]

  if (length(name_ ) > 1) {
    stop("More than one first order term")
  }

  # Remove intercept
  attr(aterms, "intercept") <- 0

  mm <- cbind(name_ = 1, model_data)
  names(mm)[1] <- name_
  model.matrix(aterms, mm) %*% cc[tl]
}
