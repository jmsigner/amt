
data(fisher)
set.seed(123)
mini_fisher <- amt_fisher[sample(nrow(amt_fisher), 150), ]


test_that("Create steps", {
  expect_is(mini_fisher %>% hr_mcp, "mcp")
  expect_is(mini_fisher %>% hr_mcp, "hr")
  expect_is(mini_fisher %>% hr_kde, "kde")
  expect_is(mini_fisher %>% hr_kde, "hr")
  expect_is(mini_fisher %>% hr_locoh, "locoh")
  expect_is(mini_fisher %>% hr_locoh, "hr")
})

