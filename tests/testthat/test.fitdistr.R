data(deer)
d1 <- deer %>% steps()
d2 <- deer %>% steps_by_burst() %>% mutate(a = 1, b = 2) %>%
  random_steps(ta_distr = "vonmises", sl_dist = "gamma")
d3 <- d2 %>% fit_clogit(case_ ~ sl_ + strata(step_id_))

testthat::test_that("fit_distr for ta and sl work", {
  expect_equal(sl_distr(d2), "gamma")
  expect_equal(ta_distr(d2), "vonmises")

  expect_equal(sl_distr(d3), "gamma")
  expect_equal(ta_distr(d3), "vonmises")

  expect_is(ta_kappa(d2), "numeric")
  expect_is(sl_scale(d2), "numeric")
  expect_is(sl_shape(d2), "numeric")

  expect_is(ta_kappa(d3), "numeric")
  expect_is(sl_scale(d3), "numeric")
  expect_is(sl_shape(d3), "numeric")
})
