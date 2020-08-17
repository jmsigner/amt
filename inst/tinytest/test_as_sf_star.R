library(amt)

t1 <- track(1:15, 1:15)
t2 <- track(1:15, 1:15, id = 1:15)
t3 <- track(1:15, 1:15, crs = CRS("+init=epsg:4326"))
t4 <- track(1:15, 1:15, id = 1:15, crs = CRS("+init=epsg:4326"))

p1 <- as_sf_points(t1)
p2 <- as_sf_points(t2)
p3 <- as_sf_points(t3)
p4 <- as_sf_points(t4)

tinytest::expect_equivalent(nrow(p1), 15)
tinytest::expect_equivalent(nrow(p2), 15)
tinytest::expect_equivalent(nrow(p3), 15)
tinytest::expect_equivalent(nrow(p4), 15)

tinytest::expect_equivalent(ncol(p1), 1)
tinytest::expect_equivalent(ncol(p2), 2)
tinytest::expect_equivalent(ncol(p3), 1)
tinytest::expect_equivalent(ncol(p4), 2)

tinytest::expect_true(sf::st_crs(p1) == sf::NA_crs_)
tinytest::expect_true(sf::st_crs(p2) == sf::NA_crs_)
tinytest::expect_false(sf::st_crs(p3) == sf::NA_crs_)
tinytest::expect_false(sf::st_crs(p4) == sf::NA_crs_)

