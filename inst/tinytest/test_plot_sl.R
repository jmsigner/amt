library(amt)

# Use the same object to check each stored distribution against base R.
random <- data.frame(sl_ = c(1, 2), case_ = c(TRUE, FALSE))
class(random) <- c("random_steps", class(random))

check_density <- function(distribution, quantile, density, from = 0) {
  attr(random, "sl_") <- distribution
  result <- plot_sl(random, n = 41, upper_quantile = 0.95, plot = FALSE)
  expect_equal(result$sl[1], from)
  expect_equal(tail(result$sl, 1), quantile)
  expect_equal(result$d, density(result$sl))
}

expect_equal(sort(available_distr("sl", names_only = TRUE)),
             sort(c("gamma", "exp", "lnorm", "hnorm", "unif")))

check_density(make_gamma_distr(shape = 2, scale = 3),
              qgamma(0.95, shape = 2, scale = 3),
              function(x) dgamma(x, shape = 2, scale = 3))
check_density(make_distribution("gamma", list(shape = 2, rate = 0.5)),
              qgamma(0.95, shape = 2, rate = 0.5),
              function(x) dgamma(x, shape = 2, rate = 0.5))
check_density(make_exp_distr(rate = 0.5), qexp(0.95, rate = 0.5),
              function(x) dexp(x, rate = 0.5))
check_density(make_lnorm_distr(meanlog = 1, sdlog = 0.7),
              qlnorm(0.95, meanlog = 1, sdlog = 0.7),
              function(x) dlnorm(x, meanlog = 1, sdlog = 0.7))
check_density(make_hnorm_distr(sd = 2), qnorm(0.975, sd = 2),
              function(x) 2 * dnorm(x, sd = 2))
check_density(make_unif_distr(min = 2, max = 10), qunif(0.95, 2, 10),
              function(x) dunif(x, 2, 10))
check_density(make_unif_distr(min = -2, max = 10), qunif(0.95, -2, 10),
              function(x) dunif(x, -2, 10), from = -2)

# Uniform densities with a positive lower bound must show the jump accurately.
attr(random, "sl_") <- make_unif_distr(min = 2, max = 10)
uniform <- plot_sl(random, n = 41, plot = FALSE)
at_bound <- which(uniform$sl == 2)[1]
expect_true(uniform$sl[at_bound - 1] < 2)
expect_equal(uniform$d[at_bound - 1], 0)
expect_equal(uniform$d[at_bound], 1 / 8)

# Keep the original gamma grid and density for the default arguments.
attr(random, "sl_") <- make_gamma_distr(shape = 2, scale = 3)
gamma <- plot_sl(random, plot = FALSE)
gamma_x <- seq(0, qgamma(0.99, shape = 2, scale = 3), length.out = 1000)
expect_equal(gamma$sl, gamma_x)
expect_equal(gamma$d, dgamma(gamma_x, shape = 2, scale = 3))

# Both object classes use the stored step-length distribution.
attr(random, "sl_") <- make_exp_distr(rate = 0.5)
model <- structure(list(sl_ = make_exp_distr(rate = 0.5)), class = "fit_clogit")
expect_equal(plot_sl(model, plot = FALSE), plot_sl(random, plot = FALSE))

if (requireNamespace("ggplot2", quietly = TRUE)) {
  p <- plot_sl(random, engine = "ggplot2", plot = FALSE)
  expect_true(inherits(p, "ggplot"))
  expect_equal(p$data, plot_sl(random, plot = FALSE))
  expect_equal(p$labels$y, "Density")
}

# Exercise the integration with steps produced by the package.
data(deer)
set.seed(10)
st <- deer[1:100, ] |> filter_min_n_burst(min_n = 4) |> steps_by_burst()
sl_exp <- fit_distr(st$sl_, "exp")
sampled <- random_steps(st, sl_distr = sl_exp, n_control = 2)
sampled_density <- plot_sl(sampled, n = 25, plot = FALSE)
expect_equal(sampled_density$d, dexp(sampled_density$sl, rate = sl_exp$params$rate))
