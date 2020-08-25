# Simulate data
library(amt)

data(deer)
suppressWarnings(ds <- steps(deer))

tinytest::expect_equivalent(deer$x_, as_track(ds)$x_)
tinytest::expect_equivalent(deer$y_, as_track(ds)$y_)
tinytest::expect_equivalent(deer$t_, as_track(ds)$t_)
tinytest::expect_equivalent(get_crs(deer), get_crs(as_track(ds)))

# when crating steps by burst, we lose some points (i.e., when end is not a new start)
d1 <- filter_min_n_burst(deer, n = 3)
d1s <- steps_by_burst(d1)

tinytest::expect_equivalent(d1$x_, as_track(d1s)$x_)
tinytest::expect_equivalent(d1$y_, as_track(d1s)$y_)
tinytest::expect_equivalent(d1$t_, as_track(d1s)$t_)
tinytest::expect_equivalent(d1$burst_, as_track(d1s)$burst_)
tinytest::expect_equivalent(get_crs(d1), get_crs(as_track(d1s)))
