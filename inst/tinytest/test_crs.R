# Simulate data
library(amt)
library(lubridate)

set.seed(123)

trk <- tibble(x = cumsum(rnorm(20)), y = cumsum(rnorm(20)),
              ts = ymd_hm("2019-01-01 00:00") + hours(0:19))
t1 <- make_track(trk, x, y, ts)
s1 <- steps(t1)
r1 <- random_points(t1)
h1.1<- hr_mcp(t1)
h1.2 <- hr_kde(t1)


data(deer)
t2 <- deer[1:100, ]
s2 <- steps(t2)
h2.1 <- hr_mcp(t2)
h2.2 <- hr_kde(t2)
h2.3 <- hr_akde(t2)

# get crs
expect_true(is.na(get_crs(t1)))
expect_true(is.na(get_crs(s1)))
expect_true(is.na(get_crs(h1.1)))
expect_true(is.na(get_crs(h1.2)))

expect_true(is(get_crs(t2), "CRS"))
expect_true(is(get_crs(s2), "CRS"))
expect_true(is(get_crs(h2.1), "crs"))
expect_true(is(get_crs(h2.2), "crs"))
expect_true(is(get_crs(h2.3), "crs"))

expect_false(has_crs(t1))
expect_false(has_crs(s1))
expect_false(has_crs(h1.1))
expect_false(has_crs(h1.2))

expect_true(has_crs(t2))
expect_true(has_crs(s2))
expect_true(has_crs(h2.1))
expect_true(has_crs(h2.2))
expect_true(has_crs(h2.3))

