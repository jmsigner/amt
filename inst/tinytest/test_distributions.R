library(amt)

ed1 <- make_distribution("exp", params = list(rate = 0.5))
ed2 <- make_exp_distr(0.5)

expect_equal(ed1, ed2)

# random numbers
expect_equal({set.seed(123); rexp(10, 0.5)},
             {set.seed(123); random_numbers(ed1, 10)})


# random numbers
expect_equal({set.seed(123); rexp(10, 0.5)},
             {set.seed(123); random_numbers(ed1, 10)})

# Gamma

# fitted models
data(deer)
d2 <- deer %>% steps_by_burst() %>% mutate(a = 1, b = 2) %>%
  random_steps()
d3 <- d2 %>% fit_clogit(case_ ~ sl_ + strata(step_id_))

expect_equal(sl_distr_name(d2), "gamma")
expect_equal(ta_distr_name(d2), "vonmises")

expect_equal(sl_distr_name(d3), "gamma")
expect_equal(ta_distr_name(d3), "vonmises")

expect_true(is(sl_distr_params(d2), "list"))
expect_true(is(ta_distr_params(d2), "list"))

expect_true(is(sl_distr_params(d3), "list"))
expect_true(is(ta_distr_params(d3), "list"))

