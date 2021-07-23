# Simulate data
library(amt)

data(deer)
suppressWarnings(ds <- steps(deer))

expect_equivalent(deer$x_, as_track(ds)$x_)
expect_equivalent(deer$y_, as_track(ds)$y_)
expect_equivalent(deer$t_, as_track(ds)$t_)
expect_equivalent(get_crs(deer), get_crs(as_track(ds)))

# when crating steps by burst, we lose some points (i.e., when end is not a new start)
d1 <- filter_min_n_burst(deer, n = 3)
d1s <- steps_by_burst(d1)

expect_equivalent(d1$x_, as_track(d1s)$x_)
expect_equivalent(d1$y_, as_track(d1s)$y_)
expect_equivalent(d1$t_, as_track(d1s)$t_)
expect_equivalent(d1$burst_, as_track(d1s)$burst_)
expect_equivalent(get_crs(d1), get_crs(as_track(d1s)))

# As track from data.frame
dat <- data.frame(
  x_ = 1:10, y_ = 1:10,
  t_ = lubridate::ymd("2020-02-01") + days(0:9))

expect_equivalent(make_track(dat, x_, y_), as_track(dat[, c("x_", "y_")]))
expect_equivalent(make_track(dat, x_, y_, t_), as_track(dat[, c("x_", "y_", "t_")]))

