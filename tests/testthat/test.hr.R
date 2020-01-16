
data(fisher)
set.seed(123)
mini_fisher <- amt_fisher[sample(nrow(amt_fisher), 150), ]


test_that("MPC and KDE have the right class", {
  expect_is(mini_fisher %>% hr_mcp, "mcp")
  expect_is(mini_fisher %>% hr_mcp, "hr")
  expect_is(mini_fisher %>% hr_kde, "kde")
  expect_is(mini_fisher %>% hr_kde, "hr")
  expect_is(mini_fisher %>% hr_locoh, "locoh")
  expect_is(mini_fisher %>% hr_locoh, "hr")
})
class(mini_fisher %>% hr_akde)

test_that("akde works", {
  expect_is(mini_fisher %>% hr_akde, "akde")
  expect_is(mini_fisher %>% hr_akde, "hr")
  expect_warning(
    expect_error(mini_fisher %>% hr_akde(., model = fit_ctmm(., "bm")))
  )

})
