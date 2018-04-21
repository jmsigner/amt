# library(amt)
# library(lubridate)
#
# context("Steps")
#
# dat <- data_frame(id = rep(1:3, each = 3), x = 1:9, y = 1:9,
#                   t = as.POSIXct(ymd("2018-01-01") + c(hours(1:3), days(1:3), weeks(1:3))))
#
# x1 <- mk_track(dat, x, y, t, id = id)
#
# s1 <- x1 %>% group_by(id) %>% nest(-id) %>%
#   mutate(steps = map(data, steps)) %>% select(id, steps) %>% unnest()
#
# s2 <- x1 %>% group_by(id) %>% nest(-id) %>%
#   mutate(steps = map(data, steps, diff_time_units = "hours")) %>% select(id, steps) %>% unnest()
#
# s3 <- x1 %>% group_by(id) %>% nest(-id) %>%
#   mutate(steps = map(data, steps, diff_time_units = "mins")) %>% select(id, steps) %>% unnest()
#
# test_that("tracks are created correctly", {
#   expect_equal(attr(s1$dt_, "units"), "secs")
#   expect_equal(attr(s2$dt_, "units"), "hours")
#   expect_equal(attr(s3$dt_, "units"), "mins")
# })
#
#
