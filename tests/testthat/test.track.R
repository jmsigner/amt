# library(amt)
#
# context("Track")
#
# dat <- data_frame(id = 34, x = 1:10, burst = 3, y = 1:10, z = 1:10, t = 3)
#
# x1 <- mk_track(dat, x, y)
# x2 <- mk_track(dat, x, y, id = id)
# x3 <- mk_track(dat, x, y, id = id, burst = burst)
# x4 <- mk_track(dat, x, y, all_cols = TRUE)
#
# dat <- data_frame(x = 1:7, y = 1:7,
#                   t = lubridate::ymd("2018-01-01") + lubridate::days(c(1, 2, 2, 3, 4, 4, 5)),
#                   x1 = 3, x2 = 4, z1 = 32)
#
# test_that("tracks are created correctly", {
#   expect_is(x1, "track_xy")
#   expect_is(x1, "data.frame")
#   expect_is(x2, "track_xy")
#   expect_is(x2, "data.frame")
#   expect_is(x3, "track_xy")
#   expect_is(x3, "data.frame")
#   expect_is(x4, "data.frame")
#   expect_is(x4, "track_xy")
#
#   expect_equal(ncol(x4), 6)
# })
#
# dat <- data_frame(x = 1:7, y = 1:7,
#                   t = lubridate::ymd("2018-01-01") + lubridate::days(c(1, 2, 2, 3, 4, 4, 5)),
#                   x1 = 3, x2 = 4, z1 = 32)
# tr1 <- make_track(dat, x, y, t, all_cols = TRUE)
# tr2 <- make_track(dat, x, y, t, all_cols = FALSE)
# tr3 <- make_track(dat, x, y, t)
# tr1a <- make_track(dat, x, y, all_cols = TRUE)
# tr2a <- make_track(dat, x, y, all_cols = FALSE)
# tr3a <- make_track(dat, x, y)
#
# test_that("tracks are created correctly", {
#   expect_is(tr1, "track_xy")
#   expect_is(tr2, "track_xy")
#   expect_is(tr3, "track_xy")
#   expect_is(tr1, "track_xyt")
#   expect_is(tr2, "track_xyt")
#   expect_is(tr3, "track_xyt")
#   expect_is(tr1a, "track_xy")
#   expect_is(tr2a, "track_xy")
#   expect_is(tr2a, "track_xy")
#
#   expect_equal(ncol(tr1), 6)
#   expect_equal(ncol(tr2), 3)
#   expect_equal(ncol(tr3), 3)
#   expect_equal(ncol(tr1a), 6)
#   expect_equal(ncol(tr2a), 2)
#   expect_equal(ncol(tr3a), 2)
#
#   expect_error(make_track(dat, x, y, t, check_duplicates = TRUE))
#   expect_error(make_track(dat, x, y, t, check_duplicates = TRUE, all_cols = TRUE))
#
#   expect_is(make_track(dat, x, y, check_duplicates = TRUE), "track_xy")
#   expect_is(make_track(dat, x, y, check_duplicates = TRUE, all_cols = TRUE), "track_xy")
# })
#
# #
# #
# # test_that("dplyr verbs work", {
# #   # arrange
# #   expect_is(arrange(x1, x_), "track_xy")
# #   expect_is(arrange(x1, x_), "data.frame")
# #   expect_is(arrange(x2, x_), "track_xyt")
# #   expect_is(arrange(x2, x_), "track_xy")
# #   expect_is(arrange(x2, x_), "data.frame")
# #   expect_is(arrange(x3, x_), "track_xyt")
# #   expect_is(arrange(x3, x_), "track_xy")
# #   expect_is(arrange(x3, x_), "data.frame")
# #
# #   # filter
# #   expect_is(filter(x1, x_ > 3), "track_xy")
# #   expect_is(filter(x1, x_ > 3), "data.frame")
# #   expect_is(filter(x2, x_ > 3), "track_xyt")
# #   expect_is(filter(x2, x_ > 3), "track_xy")
# #   expect_is(filter(x2, x_ > 3), "data.frame")
# #   expect_is(filter(x3, x_ > 3), "track_xyt")
# #   expect_is(filter(x3, x_ > 3), "track_xy")
# #   expect_is(filter(x3, x_ > 3), "data.frame")
# #
# #   # group_by
# #   expect_is(group_by(x1, x_), "track_xy")
# #   expect_is(group_by(x1, x_), "data.frame")
# #   expect_is(group_by(x2, x_), "track_xyt")
# #   expect_is(group_by(x2, x_), "track_xy")
# #   expect_is(group_by(x2, x_), "data.frame")
# #   expect_is(group_by(x3, x_), "track_xyt")
# #   expect_is(group_by(x3, x_), "track_xy")
# #   expect_is(group_by(x3, x_), "data.frame")
# #
# #   # select
# #   expect_is(select(x1, x_, y_), "track_xy")
# #   expect_is(select(x1, x_, y_), "data.frame")
# #   expect_is(select(x2, x_, y_), "track_xyt")
# #   expect_is(select(x2, x_, y_), "track_xy")
# #   expect_is(select(x2, x_, y_), "data.frame")
# #   expect_is(select(x3, x_, y_), "track_xyt")
# #   expect_is(select(x3, x_, y_), "track_xy")
# #   expect_is(select(x3, x_, y_), "data.frame")
# #
# #   # summarise
# #   expect_is(summarise(x1, mean(x_)), "track_xy")
# #   expect_is(summarise(x1, mean(x_)), "data.frame")
# #   expect_is(summarise(x2, mean(x_)), "track_xyt")
# #   expect_is(summarise(x2, mean(x_)), "track_xy")
# #   expect_is(summarise(x2, mean(x_)), "data.frame")
# #   expect_is(summarise(x3, mean(x_)), "track_xyt")
# #   expect_is(summarise(x3, mean(x_)), "track_xy")
# #   expect_is(summarise(x3, mean(x_)), "data.frame")
# #
# #   # summarize
# #   expect_is(summarize(x1, mean(x_)), "track_xy")
# #   expect_is(summarize(x1, mean(x_)), "data.frame")
# #   expect_is(summarize(x2, mean(x_)), "track_xyt")
# #   expect_is(summarize(x2, mean(x_)), "track_xy")
# #   expect_is(summarize(x2, mean(x_)), "data.frame")
# #   expect_is(summarize(x3, mean(x_)), "track_xyt")
# #   expect_is(summarize(x3, mean(x_)), "track_xy")
# #   expect_is(summarize(x3, mean(x_)), "data.frame")
# # })
# #
#
