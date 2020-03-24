# simulate data
set.seed(123)
dat <- tibble(x = cumsum(rnorm(10)),
              y = cumsum(rnorm(10)),
              t = lubridate::ymd("2020-01-01") + days(1:10))

trk <- make_track(dat, x, y, t, crs = CRS("+init=epsg:4326"))
trk_ctmm <- as_telemetry(trk)
g <- ctmm::ctmm.guess(trk_ctmm, interactive = FALSE)



# iid
expect_equal(
  fit_ctmm(trk, "iid"),
  ctmm::ctmm.fit(trk_ctmm, ctmm::ctmm(tau = NULL)))
# bm
expect_equal(
  fit_ctmm(trk, "bm"),
  ctmm::ctmm.fit(trk_ctmm, ctmm::ctmm(tau = Inf)))
# ou
# expect_equal(
#   {set.seed(123); fit_ctmm(trk, "ou")},
#   {set.seed(123); ctmm::ctmm.fit(trk_ctmm, ctmm::ctmm(tau = g$tau[1]))})
# # ouf
# expect_equal(
#   {set.seed(123); fit_ctmm(trk, "ouf")},
#   {set.seed(123); ctmm::ctmm.fit(trk_ctmm, ctmm::ctmm(tau = g$tau[1:2]))})
# # auto
# expect_equal(
#   {set.seed(123); fit_ctmm(trk, "auto")},
#   {set.seed(123); ctmm::ctmm.select(trk_ctmm, g)})
