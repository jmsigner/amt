library(lubridate)

data(sh)
d1 <- make_track(sh, x_epsg31467, y_epsg31467)
d2 <- sh %>% mutate(ts = as.POSIXct(ymd(day) + hms(time))) %>%  make_track(x_epsg31467, y_epsg31467, ts)
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
