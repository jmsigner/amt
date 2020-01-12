data(deer)
d1 <- deer %>% steps()
d2 <- deer %>% steps_by_burst() %>% mutate(a = 1, b = 2) %>%
  random_steps()
d3 <- d2 %>% fit_clogit(case_ ~ sl_ + strata(step_id_))

testthat::test_that("fit_distr for ta and sl work", {
  expect_equal(sl_distr_name(d2), "gamma")
  expect_equal(ta_distr_name(d2), "vonmises")

  expect_equal(sl_distr_name(d3), "gamma")
  expect_equal(ta_distr_name(d3), "vonmises")

  expect_is(sl_distr_params(d2), "list")
  expect_is(ta_distr_params(d2), "list")

  expect_is(sl_distr_params(d3), "list")
  expect_is(ta_distr_params(d3), "list")
})
