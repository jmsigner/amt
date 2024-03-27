library(amt)

data(deer)

expect_equivalent(class(make_trast(deer)), "SpatRaster")
expect_equivalent(terra::res(make_trast(deer, res = 1e3)), rep(1e3, 2))
expect_equivalent(terra::res(make_trast(deer, res = 4e3)), rep(4e3, 2))
