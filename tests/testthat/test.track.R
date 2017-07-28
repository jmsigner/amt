library(amt)

context("Track")
#
# dat <- data_frame(id = 34, x = 1:10, burst = 3, y = 1:10, z = 1:10, t = 3)
#
# x1 <- mk_track(dat, x, y)
# x2 <- mk_track(dat, x, y, id = id)
# x3 <- mk_track(dat, x, y, id = id, burst = burst)
#
# test_that("tracks are created correctly", {
#   expect_is(x1, "track_xy")
#   expect_is(x1, "data.frame")
#   expect_is(x2, "track_xyt")
#   expect_is(x2, "track_xy")
#   expect_is(x2, "data.frame")
#   expect_is(x3, "track_xyt")
#   expect_is(x3, "track_xy")
#   expect_is(x3, "data.frame")
# })
#
#
#
# test_that("dplyr verbs work", {
#   # arrange
#   expect_is(arrange(x1, x_), "track_xy")
#   expect_is(arrange(x1, x_), "data.frame")
#   expect_is(arrange(x2, x_), "track_xyt")
#   expect_is(arrange(x2, x_), "track_xy")
#   expect_is(arrange(x2, x_), "data.frame")
#   expect_is(arrange(x3, x_), "track_xyt")
#   expect_is(arrange(x3, x_), "track_xy")
#   expect_is(arrange(x3, x_), "data.frame")
#
#   # filter
#   expect_is(filter(x1, x_ > 3), "track_xy")
#   expect_is(filter(x1, x_ > 3), "data.frame")
#   expect_is(filter(x2, x_ > 3), "track_xyt")
#   expect_is(filter(x2, x_ > 3), "track_xy")
#   expect_is(filter(x2, x_ > 3), "data.frame")
#   expect_is(filter(x3, x_ > 3), "track_xyt")
#   expect_is(filter(x3, x_ > 3), "track_xy")
#   expect_is(filter(x3, x_ > 3), "data.frame")
#
#   # group_by
#   expect_is(group_by(x1, x_), "track_xy")
#   expect_is(group_by(x1, x_), "data.frame")
#   expect_is(group_by(x2, x_), "track_xyt")
#   expect_is(group_by(x2, x_), "track_xy")
#   expect_is(group_by(x2, x_), "data.frame")
#   expect_is(group_by(x3, x_), "track_xyt")
#   expect_is(group_by(x3, x_), "track_xy")
#   expect_is(group_by(x3, x_), "data.frame")
#
#   # select
#   expect_is(select(x1, x_, y_), "track_xy")
#   expect_is(select(x1, x_, y_), "data.frame")
#   expect_is(select(x2, x_, y_), "track_xyt")
#   expect_is(select(x2, x_, y_), "track_xy")
#   expect_is(select(x2, x_, y_), "data.frame")
#   expect_is(select(x3, x_, y_), "track_xyt")
#   expect_is(select(x3, x_, y_), "track_xy")
#   expect_is(select(x3, x_, y_), "data.frame")
#
#   # summarise
#   expect_is(summarise(x1, mean(x_)), "track_xy")
#   expect_is(summarise(x1, mean(x_)), "data.frame")
#   expect_is(summarise(x2, mean(x_)), "track_xyt")
#   expect_is(summarise(x2, mean(x_)), "track_xy")
#   expect_is(summarise(x2, mean(x_)), "data.frame")
#   expect_is(summarise(x3, mean(x_)), "track_xyt")
#   expect_is(summarise(x3, mean(x_)), "track_xy")
#   expect_is(summarise(x3, mean(x_)), "data.frame")
#
#   # summarize
#   expect_is(summarize(x1, mean(x_)), "track_xy")
#   expect_is(summarize(x1, mean(x_)), "data.frame")
#   expect_is(summarize(x2, mean(x_)), "track_xyt")
#   expect_is(summarize(x2, mean(x_)), "track_xy")
#   expect_is(summarize(x2, mean(x_)), "data.frame")
#   expect_is(summarize(x3, mean(x_)), "track_xyt")
#   expect_is(summarize(x3, mean(x_)), "track_xy")
#   expect_is(summarize(x3, mean(x_)), "data.frame")
# })
#
