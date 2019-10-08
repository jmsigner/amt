
library(Rcpp)

sourceCpp("src/simulate2.cpp")

library(raster)

hab <- raster(xmn = 0, xmx = 1e4, ymn = 0, ymx = 1e4, res = 1)
hab[] <- runif(ncell(hab))
dk <- raster(xmn = -100, xmx = 100, ymn = -100, ymx = 100, res = 1)
dk1 <- rasterToPoints(dk)
dk1 <- cbind(dk1, dist = sqrt(dk1[, 1]^2 + dk1[, 2]^2))
dk1 <- dk1[dk1[, "dist"] < 100, 1:2]
plot(dk1)
dk1


system.time(
  dk3 <- simulate_2(n_steps = 1000, start = cellFromXY(hab, cbind(5000, 5000)),
                    nc = ncol(hab), nr = nrow(hab), hab = rasterToPoints(hab),
                    dk = dk1)
)


plot(hab)
points(dk3)
