# Simulate data
library(amt)
library(lubridate)

dat <- tibble(x = 1:10, y = 1:10, t = ymd_hms("2020-11-18 00:00:00") + hours(0:9),
              dop = 1, HDOP = 1, hdop = 1, DOP = 1)

tr <- make_track(dat, x, y, t, all_cols = TRUE, crs = CRS("+init=epsg:4326"))

expect_true("HDOP" %in% names(as_telemetry(amt::select(tr, x_, y_, t_, dop))))
expect_true("HDOP" %in% names(as_telemetry(amt::select(tr, x_, y_, t_, hdop))))
expect_true("HDOP" %in% names(as_telemetry(amt::select(tr, x_, y_, t_, DOP))))
expect_true("HDOP" %in% names(as_telemetry(amt::select(tr, x_, y_, t_, HDOP))))
expect_false("HDOP" %in% names(as_telemetry(amt::select(tr, x_, y_, t_))))

