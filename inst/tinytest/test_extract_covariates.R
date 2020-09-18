# Simulate data
library(raster)
library(lubridate)
library(amt)

set.seed(123)

trk <- tibble(x = cumsum(rnorm(20)), y = cumsum(rnorm(20)),
              ts = ymd_hm("2019-01-01 00:00") + hours(0:19))
trk1 <- make_track(trk, x, y)
trk2 <- make_track(trk, x, y, ts)

# env covars
r <- stack(map(1:15,
    ~ raster(xmn = -100, xmx = 100, ymn = -100, ymx = 100,
             res = 1, vals = runif(4e4, ., . + 1))))

r1 <- r[[1]]
r2 <- stack(r1, r1)
rt <- setZ(r, ymd_hm("2019-01-01 00:00") + hours(0:14))

# track_xy
expect_equal(ncol(extract_covariates(trk1, r1)), 3)
expect_equal(ncol(extract_covariates(trk1, r2)), 4)
expect_equal(ncol(extract_covariates(trk1, rt)), 17)

# track_xyt
expect_equal(ncol(extract_covariates(trk2, r1)), 4)
expect_equal(ncol(extract_covariates(trk2, r2)), 5)
expect_equal(ncol(extract_covariates(trk2, rt)), 18)

# Random points
rp <- random_points(trk2)
expect_equal(ncol(extract_covariates(rp, r1)), 4)
expect_equal(ncol(extract_covariates(rp, r2)), 5)
expect_equal(ncol(extract_covariates(rp, rt)), 18)

# steps
s1 <- trk1 %>% steps()
s2 <- trk2 %>% steps()

expect_equal(ncol(extract_covariates(s1, r1, where = "start")), 8)
expect_equal(ncol(extract_covariates(s1, r1, where = "end")), 8)
expect_equal(ncol(extract_covariates(s1, r1, where = "both")), 9)
expect_equal(ncol(extract_covariates(s1, r2, where = "start")), 9)
expect_equal(ncol(extract_covariates(s1, r2, where = "end")), 9)
expect_equal(ncol(extract_covariates(s1, r2, where = "both")), 11)
expect_equal(ncol(extract_covariates(s1, rt, where = "start")), 22)
expect_equal(ncol(extract_covariates(s1, rt, where = "end")), 22)
expect_equal(ncol(extract_covariates(s1, rt, where = "both")), 37)
expect_equal(ncol(extract_covariates(s2, r1, where = "start")), 11)
expect_equal(ncol(extract_covariates(s2, r1, where = "end")), 11)
expect_equal(ncol(extract_covariates(s2, r1, where = "both")), 12)
expect_equal(ncol(extract_covariates(s2, r2, where = "start")), 12)
expect_equal(ncol(extract_covariates(s2, r2, where = "end")), 12)
expect_equal(ncol(extract_covariates(s2, r2, where = "both")), 14)
expect_equal(ncol(extract_covariates(s2, rt, where = "start")), 25)
expect_equal(ncol(extract_covariates(s2, rt, where = "end")), 25)
expect_equal(ncol(extract_covariates(s2, rt, where = "both")), 40)

# along
expect_error(extract_covariates_along(trk1, r1))
expect_error(extract_covariates_along(trk2, r1))
expect_true(is(extract_covariates_along(s1, r1), "list"))
expect_true(is(extract_covariates_along(s2, r1), "list"))

expect_true(is(extract_covariates_along(s2, r2), "list"))
expect_true(is(extract_covariates_along(s2, rt), "list"))

# varying time
expect_error(extract_covariates_var_time(trk1, r1))
expect_error(extract_covariates_var_time(trk2, r1))
expect_error(extract_covariates_var_time(s1, r1))
expect_error(extract_covariates_var_time(s2, r1))

expect_equal(
  ncol(extract_covariates_var_time(trk2, rt, max_time = minutes(120))), 4)
expect_equal(
  ncol(extract_covariates_var_time(s2, rt, max_time = minutes(120))), 11)
expect_equal(
  ncol(extract_covariates_var_time(s2, rt, max_time = minutes(120), where = "end")),
  11)
expect_equal(
  ncol(extract_covariates_var_time(s2, rt, max_time = minutes(120), where = "start")),
  11)
expect_equal(
  ncol(extract_covariates_var_time(s2, rt, max_time = minutes(120), where = "both")),
  12)

