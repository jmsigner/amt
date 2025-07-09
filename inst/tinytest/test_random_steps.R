library(amt)
# Check that angles are correct
data(deer)
a <- round(-pi, 5)
x1 <- random_steps(c(0, 0), n_control = 1, rand_sl = 1, rand_ta = a)
expect_true(round(atan2(x1[1, "y2_"] - x1[1, "y1_"], x1[1, "x2_"] - x1[1, "x1_"]), 5) == a)


a <- round(-pi/2, 5)
x1 <- random_steps(c(0, 0), n_control = 1, rand_sl = 1, rand_ta = a)
expect_true(round(atan2(x1[1, "y2_"] - x1[1, "y1_"], x1[1, "x2_"] - x1[1, "x1_"]), 5) == a)


a <- round(-pi/3, 5)
x1 <- random_steps(c(0, 0), n_control = 1, rand_sl = 1, rand_ta = a)
expect_true(round(atan2(x1[1, "y2_"] - x1[1, "y1_"], x1[1, "x2_"] - x1[1, "x1_"]), 5) == a)


a <- round(-pi/4, 5)
x1 <- random_steps(c(0, 0), n_control = 1, rand_sl = 1, rand_ta = a)
expect_true(round(atan2(x1[1, "y2_"] - x1[1, "y1_"], x1[1, "x2_"] - x1[1, "x1_"]), 5) == a)

a <- round(-pi/5, 5)
x1 <- random_steps(c(0, 0), n_control = 1, rand_sl = 1, rand_ta = a)
expect_true(round(atan2(x1[1, "y2_"] - x1[1, "y1_"], x1[1, "x2_"] - x1[1, "x1_"]), 5) == a)

a <- round(0, 5)
x1 <- random_steps(c(0, 0), n_control = 1, rand_sl = 1, rand_ta = a)
expect_true(round(atan2(x1[1, "y2_"] - x1[1, "y1_"], x1[1, "x2_"] - x1[1, "x1_"]), 5) == a)


a <- round(pi, 5)
x1 <- random_steps(c(0, 0), n_control = 1, rand_sl = 1, rand_ta = a)
expect_true(round(atan2(x1[1, "y2_"] - x1[1, "y1_"], x1[1, "x2_"] - x1[1, "x1_"]), 5) == a)


a <- round(pi/2, 5)
x1 <- random_steps(c(0, 0), n_control = 1, rand_sl = 1, rand_ta = a)
expect_true(round(atan2(x1[1, "y2_"] - x1[1, "y1_"], x1[1, "x2_"] - x1[1, "x1_"]), 5) == a)


a <- round(pi/3, 5)
x1 <- random_steps(c(0, 0), n_control = 1, rand_sl = 1, rand_ta = a)
expect_true(round(atan2(x1[1, "y2_"] - x1[1, "y1_"], x1[1, "x2_"] - x1[1, "x1_"]), 5) == a)


a <- round(pi/4, 5)
x1 <- random_steps(c(0, 0), n_control = 1, rand_sl = 1, rand_ta = a)
expect_true(round(atan2(x1[1, "y2_"] - x1[1, "y1_"], x1[1, "x2_"] - x1[1, "x1_"]), 5) == a)

a <- round(pi/5, 5)
x1 <- random_steps(c(0, 0), n_control = 1, rand_sl = 1, rand_ta = a)
expect_true(round(atan2(x1[1, "y2_"] - x1[1, "y1_"], x1[1, "x2_"] - x1[1, "x1_"]), 5) == a)

# check args
expect_error(random_steps(c(0), n_control = 1, rand_sl = 1, rand_ta = 0))
expect_error(random_steps(c(0, 0), n_control = 1.5, rand_sl = 1, rand_ta = 0))
expect_error(random_steps(c(0, 0), n_control = -1, rand_sl = 1, rand_ta = 0))
expect_error(random_steps(c(0, 0), n_control = 1, rand_sl = -11, rand_ta = 0))
expect_error(random_steps(c(0, 0), n_control = 1, rand_sl = 11, rand_ta = -10))
expect_error(random_steps(c(0, 0), n_control = 1, rand_sl = 11, rand_ta = 10))
expect_error(random_steps(c(0, 0), n_control = 1, rand_sl = 11, rand_ta = c(-10, 0)))

# Check that it works with `steps`
xy <- data.frame(x = c(1, 10, 50, 90), y = -c(10, -10, 10, 90))
xx <- make_track(xy, x, y)
s1 <- xx |> steps()

expect_error(random_steps(xx, sl_distr = make_unif_distr(10, 100), ta_distr = make_vonmises_distr(kappa = 5)))
expect_error(random_steps(s1, 1.5, sl_distr = make_unif_distr(10, 100), ta_distr = make_vonmises_distr(kappa = 5)))


xx <- random_steps(s1, 1, sl_distr = make_unif_distr(10, 100), ta_distr = make_vonmises_distr(kappa = 5))
expect_equal(nrow(xx), 4)
expect_equal(sum(!xx$case_), 2)

xx <- random_steps(s1, 10, sl_distr = make_unif_distr(10, 100), ta_distr = make_vonmises_distr(kappa = 5))
expect_equal(nrow(xx), 22)
expect_equal(sum(!xx$case_), 20)


# Check steps are correct, i.e., random steps have the right direction
xy <- data.frame(x = 1:3, y = 1:3)
xx <- make_track(xy, x, y)
s1 <- xx |> steps()

rs <- random_steps(s1, 1, sl_distr = make_unif_distr(sqrt(2), sqrt(2)),
             ta_distr = make_unif_distr(0, 0))

# expect_equivalent(rs[1, 1:6], rs[2, 1:6])
# expect_equal(rs$case_, c(TRUE, FALSE))


# now for many steps, this needs a bit more thoughts for good tests
xy <- data.frame(x = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 4, 4),
                 y = c(1, 2, 2, 1, 1, 0, 0, 1, 1, 1, 0))
xx <- make_track(xy, x, y)
s1 <- xx |> steps()

rs <- random_steps(s1, 1, sl_distr = make_unif_distr(1, 1),
             ta_distr = make_unif_distr(0, 0))

expect_true(all(rs$sl_ == 1))
expect_true(all(rs$ta_[!rs$case_] == 0))

# Test remove incomplete steps

mini_deer <- deer[1:4, ]

expect_equal(mini_deer |> steps() |> random_steps() |> nrow(), 22)
expect_equal(mini_deer |> steps() |> random_steps() |> remove_incomplete_strata() |> nrow(), 22)
expect_equal(mini_deer |> steps() |> random_steps() |> remove_incomplete_strata(col = "sl_") |> nrow(), 22)
expect_true(mini_deer |> steps_by_burst() |> nrow() == 3)

expect_equal(mini_deer |> steps() |> random_steps() |> remove_incomplete_strata(col = "sl_") |> nrow(), 22)

expect_error(mini_deer |> steps() |> random_steps() |> remove_incomplete_strata(col = "sl"))


###
mini_deer <- deer[5:20, ]
expect_true(mini_deer |> steps() |> random_steps(n_control = 1) |>
              pull(step_id_) |> unique() |> length() == nrow(mini_deer) - 2)


# More checks
expect_true(deer |> filter(burst_ %in% c(10, 13)) |> steps_by_burst() |>
  random_steps() |> is("data.frame"))

expect_error(deer |> filter(burst_ %in% c(18, 21)) |> steps_by_burst() |>
  random_steps())

expect_true(deer |> filter(burst_ == 10) |> steps_by_burst() |>
  random_steps() |> is("data.frame"))
