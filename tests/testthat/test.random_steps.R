library(amt)

context("random steps")

data(deer)

trk1 <- deer %>% make_track(x_, y_, t_)
sl <- fit_sl_dist(trk1 %>% steps(), sl_)

test_that("step length is correctly estimated", {
  expect_equal(trk1 %>% steps() %>% fit_sl_dist(sl_) %>% sl_distr(), "gamma")
  expect_equal(trk1 %>% steps() %>% fit_sl_dist() %>% sl_distr(), "gamma")
  expect_equal(trk1 %>% steps() %>% fit_sl_dist(distr = "unif") %>% sl_distr(), "unif")
  expect_equal(trk1 %>% steps() %>% fit_sl_dist(distr = "exp") %>% sl_distr(), "exp")
})
