library(amt)

context("random steps")

data(deer)

trk1 <- deer %>% make_track(x_, y_, t_)
sl <- fit_distr(trk1 %>% steps() %>% pull(sl_), "gamma")

test_that("step length is correctly estimated", {
  expect_equal(trk1 %>% steps() %>% pull(sl_) %>% fit_distr(dist_name = "gamma") %>% getElement("name"), "gamma")
  expect_equal(trk1 %>% steps() %>% pull(sl_) %>% fit_distr(dist_name = "unif") %>% getElement("name"), "unif")
  expect_equal(trk1 %>% steps() %>% pull(sl_) %>% fit_distr(dist_name = "exp") %>% getElement("name"), "exp")
})

