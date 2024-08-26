library(amt)
library(lubridate)

# Some data
dat <- data.frame(
  x = runif(100),
  y = runif(100),
  t_ = ymd_hms("1900-01-01 00:00:00") + hours(0:99)
)

dat <- dat |> make_track(x, y, t_)


# Align to every 2 hours
new.times <- seq(from(dat), to(dat), by = "2 hours")
tolerance <- 100

expect_equal(
  nrow(
    track_align(dat, new.times = new.times, tolerance = tolerance)
  ), 50)

tolerance <- 10000000

expect_equal(
  nrow(
    track_align(dat, new.times = new.times, tolerance = tolerance)
  ), 50)

