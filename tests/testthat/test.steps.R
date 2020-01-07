library(lubridate)

data(deer)
d1 <- make_track(deer, x_, y_)
d2 <- deer %>%  make_track(x_, y_, t_)
d2


test_that("Create steps", {
  expect_is(d1 %>% steps(), "steps_xy")
  expect_is(d1 %>% steps(), "data.frame")
  expect_is(d2 %>% steps(), "steps_xyt")
  expect_is(d2 %>% steps(), "steps_xy")
  expect_is(d2 %>% steps(), "data.frame")
  # random steps
  expect_is(d1 %>% steps() %>% random_steps(), "random_steps")
  expect_is(d2 %>% steps() %>% random_steps(), "random_steps")
})

