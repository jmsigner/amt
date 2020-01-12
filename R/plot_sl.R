#' Plot step-length distribution
#'
#' @param x `[fit_clogit|random_steps]` \cr A fitted step selection or random steps.
#' @param upper_quantile `[nummeric(1)=0.99]{0-1}` \cr The quantile until where the distribution should be plotted. Typically this will be `0.95` or `0.99`.
#' @param n `[numeric(1)=1000]{>0}` \cr The number of breaks between `0` and `upper_quantile`.
#' @param plot `[logical(1)=TRUE]` \cr Indicates if a plot should be drawn or not.
#' @template dots_none
#' @export
#' @name plot_sl
#' @examples
#' data(deer)
#'
#' # with random steps
#' deer %>% steps_by_burst %>% random_steps %>% plot_sl
#' deer %>% steps_by_burst %>% random_steps %>% plot_sl(upper_quantile = 0.5)
#'
#' # with fitted ssf
#' deer %>% steps_by_burst %>% random_steps %>%
#'   fit_ssf(case_ ~ sl_ + strata(step_id_)) %>% plot_sl
#'
plot_sl <- function(x, ...) {
  UseMethod("plot_sl", x)
}

#' @export
#' @rdname plot_sl
plot_sl.fit_clogit <- function(x, n = 1000, upper_quantile = 0.99, plot = TRUE, ...) {
  plot_sl_base(x, n, upper_quantile, plot, ...)
}

#' @export
#' @rdname plot_sl
plot_sl.random_steps <- function(x, n = 1000, upper_quantile = 0.99, plot = TRUE, ...) {
  plot_sl_base(x, n, upper_quantile, plot, ...)
}

plot_sl_base <- function(x, n, upper_quantile, plot, ...) {
  xx <- sl_distr_params(x)
  if (sl_distr_name(x) == "gamma") {
    to <- qgamma(upper_quantile, shape = xx$shape, scale = xx$scale)
    xs <- seq(0, to, length.out = n)
    if (plot) {
      plot(xs, ys <- dgamma(xs, shape = xx$shape, scale = xx$scale), type = "l",
           ylab = "Probability",
           xlab = "Distance")
    }
    invisible(data.frame(sl = xs, d = ys))
  } else {
    stop ("distr not implemented")
  }
}
