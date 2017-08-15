# library(amt)
#
# x <- 1:10
# y <- 1:10
# t <- 1:10
# dop <- rep(1, 10)
# dim <- rep(1, 10)
# dop[5] <- 3
#
# duration_acuracy(x, y, t, dop, dim, 2)
#
#
#
# ####
# library(lubridate)
# x <- runif(1e6)
# y <- runif(1e6)
# t <- now() + minutes(1:1e5)
# dop <- runif(1e6, 0, 10)
# dim <- sample(3, 1e6, TRUE)
#
# system.time(duration_acuracy(x, y, t, dop, dim, 10 * 60))
#
