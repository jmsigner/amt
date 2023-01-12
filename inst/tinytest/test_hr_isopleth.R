
data(deer)
library(sf)
library(terra)

set.seed(131)
l <- rgamma(1e3, shape = 20, scale = 0.5)
a <- runif(1e3, -pi, pi)

x <- cos(a) * l
y <- sin(a) * l

t1 <- tibble(x, y) |> make_track(x, y)
k1 <- tibble(x, y) |> make_track(x, y) |> hr_kde()
hr_isopleths(k1) |> st_geometry() |> plot(col = "blue")


k1 <- tibble(x, y) |> make_track(x, y) |> hr_kde(levels = c(0.1, 0.5, 0.9))
iso <- hr_isopleths(k1)
plot(iso["level"])

# One normal
x <- rnorm(1e3)
y <- rnorm(1e3)

# Not sure what is wrong here -> Problem solved and issue filed
t1 <- tibble(x, y) |> make_track(x, y)
k2 <- tibble(x, y) |> make_track(x, y) |> hr_kde()
terra::plot(k1$ud)

as.contour(hr_cud(k1), levels = 0.54) |> plot()
as.contour(hr_cud(k2), levels = c(0.1, 0.3)) |> plot()

deer |> centroid(spatial = TRUE) |> str()
