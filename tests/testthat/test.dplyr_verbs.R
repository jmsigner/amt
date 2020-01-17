library(amt)

context("dplyr verbs")

dat <- tibble(x = 1:7, y = 1:7,
              t = lubridate::ymd("2018-01-01") + lubridate::days(c(1, 2, 2, 3, 4, 4, 5)),
              x1 = 3, x2 = 4, g = c(1, 1, 1, 2, 2, 2, 3))

trk <- make_track(dat, x, y, g = g)


test_that("Summarize works for trak_xy", {
   expect_equal(trk %>% group_by(g) %>% summarize(n = n()) %>% nrow(), 3)
   expect_is(trk %>% group_by(g) %>% summarize(n = n()), "tbl_df")
   expect_is(trk %>% group_by(g) %>% summarize(n = n()), "tbl")
   expect_is(trk %>% group_by(g) %>% summarize(n = n()), "data.frame")
})

trk <- make_track(dat, x, y, t, g = g)
test_that("Summarize works for trak_xyt", {
   expect_equal(trk %>% group_by(g) %>% summarize(n = n()) %>% nrow(), 3)
   expect_is(trk %>% group_by(g) %>% summarize(n = n()), "tbl_df")
   expect_is(trk %>% group_by(g) %>% summarize(n = n()), "tbl")
   expect_is(trk %>% group_by(g) %>% summarize(n = n()), "data.frame")
})

