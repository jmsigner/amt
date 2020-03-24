library(amt)


# Some data
dat <- tibble(id = 34, x = 1:10, burst = 3, y = 1:10, z = 1:10, t = 3)
x1 <- make_track(dat, x, y)
x2 <- make_track(dat, x, y, id = id)
x3 <- make_track(dat, x, y, id = id, burst = burst)
x4 <- make_track(dat, x, y, all_cols = TRUE)

dat <- tibble(x = 1:7, y = 1:7,
              t = lubridate::ymd("2018-01-01") + lubridate::days(c(1, 2, 2, 3, 4, 4, 5)),
              x1 = 3, x2 = 4, z1 = 32)

expect_true(is(x1, "track_xy"))
expect_true(is(x1, "data.frame"))
expect_true(is(x2, "track_xy"))
expect_true(is(x2, "data.frame"))
expect_true(is(x3, "track_xy"))
expect_true(is(x3, "data.frame"))
expect_true(is(x4, "data.frame"))
expect_true(is(x4, "track_xy"))
expect_equal(ncol(x4), 6)

dat <- tibble(x = 1:7, y = 1:7,
              tm = lubridate::ymd("2018-01-01") + lubridate::days(c(1, 2, 2, 3, 4, 4, 5)),
              x1 = 3, x2 = 4, z1 = 32)
tr1 <- make_track(dat, x, y, tm, all_cols = TRUE)
tr2 <- make_track(dat, x, y, tm, all_cols = FALSE)
tr3 <- make_track(dat, x, y, tm)
tr1a <- make_track(dat, x, y, all_cols = TRUE)
tr2a <- make_track(dat, x, y, all_cols = FALSE)
tr3a <- make_track(dat, x, y)

expect_true(is(tr1, "track_xy"))
expect_true(is(tr2, "track_xy"))
expect_true(is(tr3, "track_xy"))
expect_true(is(tr1, "track_xyt"))
expect_true(is(tr2, "track_xyt"))
expect_true(is(tr3, "track_xyt"))
expect_true(is(tr1a, "track_xy"))
expect_true(is(tr2a, "track_xy"))
expect_true(is(tr2a, "track_xy"))

expect_equal(ncol(tr1), 6)
expect_equal(ncol(tr2), 3)
expect_equal(ncol(tr3), 3)
expect_equal(ncol(tr1a), 6)
expect_equal(ncol(tr2a), 2)
expect_equal(ncol(tr3a), 2)

expect_error(make_track(dat, x, y, t, check_duplicates = TRUE))
expect_error(make_track(dat, x, y, t, check_duplicates = TRUE, all_cols = TRUE))

expect_true(is(make_track(dat, x, y, check_duplicates = TRUE), "track_xy"))
expect_true(is(make_track(dat, x, y, check_duplicates = TRUE, all_cols = TRUE), "track_xy"))


# test dplyr verbs work
expect_true(is(arrange(x1, x_), "track_xy"))
expect_true(is(arrange(x1, x_), "data.frame"))
expect_true(is(arrange(x2, x_), "track_xy"))
expect_true(is(arrange(x2, x_), "track_xy"))
expect_true(is(arrange(x2, x_), "data.frame"))
expect_true(is(arrange(tr1, x_), "track_xyt"))
expect_true(is(arrange(tr1, x_), "track_xy"))
expect_true(is(arrange(tr1, x_), "data.frame"))

# filter
expect_true(is(filter(x1, x_ > 3), "track_xy"))
expect_true(is(filter(x1, x_ > 3), "data.frame"))
expect_true(is(filter(tr1, x_ > 3), "track_xyt"))
expect_true(is(filter(tr1, x_ > 3), "track_xy"))
expect_true(is(filter(tr1, x_ > 3), "data.frame"))

# group_by
expect_true(is(group_by(x1, x_), "track_xy"))
expect_true(is(group_by(x1, x_), "data.frame"))
expect_true(is(group_by(tr1, x_), "track_xyt"))
expect_true(is(group_by(tr1, x_), "track_xy"))
expect_true(is(group_by(tr1, x_), "data.frame"))

# select
expect_true(is(dplyr::select(x1, x_, y_), "track_xy"))
expect_true(is(dplyr::select(x1, x_, y_), "data.frame"))
expect_true(is(dplyr::select(tr1, x_, y_), "track_xyt"))
expect_true(is(dplyr::select(tr1, x_, y_), "track_xy"))
expect_true(is(dplyr::select(tr1, x_, y_), "data.frame"))

# summarise
expect_true(is(summarise(x1, mean(x_)), "tbl_df"))
expect_true(is(summarise(x1, mean(x_)), "tbl"))
expect_true(is(summarise(x1, mean(x_)), "data.frame"))
expect_true(is(summarise(tr1, mean(x_)), "tbl_df"))
expect_true(is(summarise(tr1, mean(x_)), "tbl"))
expect_true(is(summarise(tr1, mean(x_)), "data.frame"))


# summarize
expect_true(is(summarize(x1, mean(x_)), "tbl_df"))
expect_true(is(summarize(x1, mean(x_)), "tbl"))
expect_true(is(summarize(x1, mean(x_)), "data.frame"))
expect_true(is(summarize(tr1, mean(x_)), "tbl_df"))
expect_true(is(summarize(tr1, mean(x_)), "tbl"))
expect_true(is(summarize(tr1, mean(x_)), "data.frame"))


