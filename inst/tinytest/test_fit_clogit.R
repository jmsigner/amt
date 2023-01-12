library(amt)
d <- amt_fisher |> filter(name == "Leroy") |>
  steps() |> random_steps()

# Missing strata
f <- case_ ~ sl_ + log_sl_
expect_error(fit_clogit(d, f))
expect_error(fit_ssf(d, f))
expect_error(fit_issf(d, f))

# With strata
f <- case_ ~ sl_ + strata(step_id_)
expect_stdout(fit_clogit(d, f))
expect_stdout(fit_ssf(d, f))
expect_stdout(fit_issf(d, f))

