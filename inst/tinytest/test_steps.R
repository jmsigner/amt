data(deer)
d1 <- make_track(deer, x_, y_)
d2 <- deer |>  make_track(x_, y_, t_)
d3 <- deer |> transform_coords(4326)

expect_true(is(d1 |> steps(), "steps_xy"))
expect_true(is(d1 |> steps(), "data.frame"))
expect_true(is(d2 |> steps(), "steps_xyt"))
expect_true(is(d2 |> steps(), "steps_xy"))
expect_true(is(d2 |> steps(), "data.frame"))
expect_true(is(d1 |> steps() |> random_steps(), "random_steps"))
expect_true(is(d2 |> steps() |> random_steps(), "random_steps"))
expect_true(is(d3 |> step_lengths(lonlat = TRUE), "numeric"))

expect_equal(length(d1 |> step_lengths(lonlat = FALSE)), nrow(d1))
expect_equal(length(d2 |> step_lengths(lonlat = FALSE)), nrow(d2))
expect_equal(length(d3 |> step_lengths(lonlat = TRUE)), nrow(d3))

# Distributions
sl <- fit_distr(d2 |> steps() |> pull(sl_), "gamma")
expect_equal(d2 |> steps() |> pull(sl_) |> fit_distr(dist_name = "gamma") |> getElement("name"), "gamma")
expect_equal(d2 |> steps() |> pull(sl_) |> fit_distr(dist_name = "unif") |> getElement("name"), "unif")
expect_equal(d2 |> steps() |> pull(sl_) |> fit_distr(dist_name = "exp") |> getElement("name"), "exp")


