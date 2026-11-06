#' Plot step-length distribution
#'
#' @param x `[fit_clogit|random_steps]` \cr A fitted step selection or random steps.
#' @param upper_quantile `[numeric(1)=0.99]{0-1}` \cr The quantile until where the distribution should be plotted. Typically this will be `0.95` or `0.99`.
#' @param n `[numeric(1)=1000]{>0}` \cr The number of breaks between `0` and `upper_quantile`.
#' @param plot `[logical(1)=TRUE]` \cr Indicates if a plot should be drawn or not.
#' @param engine `[character(1)="base"]` \cr Plotting engine. `"base"` keeps
#' the existing base graphics behavior. `"ggplot2"` returns a ggplot object and
#' requires the optional `ggplot2` package.
#' @template dots_none
#' @details Gamma, exponential, log-normal, half-normal and uniform
#' step-length distributions are supported. The distribution plotted is the
#' one stored in `x`, which can be supplied by the user or fitted to data.
#' If random step lengths were supplied separately through `rand_sl`, they
#' do not necessarily follow the stored distribution.
#'
#' @return With `engine = "base"`, a data frame with the step lengths and
#' their densities, returned invisibly. With `engine = "ggplot2"`, a ggplot
#' object, returned invisibly. If `plot = TRUE`, the plot is also drawn.
#' @export
#' @name plot_sl
#' @examples
#' data(deer)
#'
#' # with random steps
#' deer[1:100, ] |> steps_by_burst() |> random_steps() |> plot_sl()
#' deer[1:100, ] |> steps_by_burst() |> random_steps() |> plot_sl(upper_quantile = 0.5)
#'
#' # Fit and plot an exponential step-length distribution
#' st <- deer[1:100, ] |> filter_min_n_burst(min_n = 4) |> steps_by_burst()
#' exp_sl <- fit_distr(st$sl_, "exp")
#' st |> random_steps(sl_distr = exp_sl) |> plot_sl()
#'
plot_sl <- function(x, ...) {
  UseMethod("plot_sl", x)
}

#' @export
#' @rdname plot_sl
plot_sl.fit_clogit <- function(x, n = 1000, upper_quantile = 0.99,
                               plot = TRUE, engine = c("base", "ggplot2"), ...) {
  plot_sl_base(
    x = x,
    n = n,
    upper_quantile = upper_quantile,
    plot = plot,
    engine = engine,
    ...
  )
}

#' @export
#' @rdname plot_sl
plot_sl.random_steps <- function(x, n = 1000, upper_quantile = 0.99,
                                 plot = TRUE, engine = c("base", "ggplot2"), ...) {
  plot_sl_base(
    x = x,
    n = n,
    upper_quantile = upper_quantile,
    plot = plot,
    engine = engine,
    ...
  )
}

plot_sl_base <- function(x, n, upper_quantile, plot, engine = c("base", "ggplot2"), ...) {
  engine <- match.arg(engine)
  name <- sl_distr_name(x)
  params <- sl_distr_params(x)

  if (identical(name, "gamma")) {
    args <- list(shape = params$shape)
    if (!is.null(params$scale)) {
      args$scale <- params$scale
    } else {
      args$rate <- params$rate
    }
    qfun <- stats::qgamma
    dfun <- stats::dgamma
  } else if (identical(name, "exp")) {
    args <- list(rate = params$rate)
    qfun <- stats::qexp
    dfun <- stats::dexp
  } else if (identical(name, "lnorm")) {
    args <- list(meanlog = params$meanlog, sdlog = params$sdlog)
    qfun <- stats::qlnorm
    dfun <- stats::dlnorm
  } else if (identical(name, "hnorm")) {
    args <- list(sd = params$sd)
    qfun <- function(p, sd) stats::qnorm((1 - p) / 2, sd = sd, lower.tail = FALSE)
    dfun <- function(x, sd) ifelse(x < 0, 0, 2 * stats::dnorm(x, sd = sd))
  } else if (identical(name, "unif")) {
    args <- list(min = params$min, max = params$max)
    qfun <- stats::qunif
    dfun <- stats::dunif
  } else {
    stop("Step-length distribution not implemented.", call. = FALSE)
  }

  to <- do.call(qfun, c(list(p = upper_quantile), args))
  from <- if (identical(name, "unif")) min(0, params$min) else 0
  xs <- seq(from, to, length.out = n)
  if (identical(name, "unif") && params$min > from && params$min < to) {
    # Include both sides of the jump at the lower limit of the support.
    just_below <- params$min - max(abs(params$min) * .Machine$double.eps,
                                   .Machine$double.xmin)
    xs <- sort(unique(c(xs, max(from, just_below), params$min)))
  }
  ys <- do.call(dfun, c(list(xs), args))
  dat <- data.frame(sl = xs, d = ys)

  if (engine == "ggplot2") {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("The `ggplot2` package is required when `engine = \"ggplot2\"`.",
           call. = FALSE)
    }
    p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$sl, y = .data$d)) +
      ggplot2::geom_line() +
      ggplot2::labs(x = "Distance", y = "Density")
    if (plot) {
      print(p)
    }
    return(invisible(p))
  }

  if (plot) {
    plot(xs, ys, type = "l", ylab = "Density", xlab = "Distance")
  }
  invisible(dat)
}
