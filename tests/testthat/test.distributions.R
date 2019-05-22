library(amt)

context("distributions")

# Set up some dummy data

test_that("Supported distributions are recognized", {
  expect_true("unif" %in% valid_ta_distr())
  expect_true("vonmises" %in% valid_ta_distr())

  expect_true("exp" %in% valid_sl_distr())
  expect_true("gamma" %in% valid_sl_distr())
  expect_true("unif" %in% valid_sl_distr())
}
)

ed1 <- make_distribution("exp", params = list(rate = 0.5))
ed2 <- make_exp_distr(0.5)

test_that("constructor for exponential distribution works", {
  expect_equal(ed1, ed2)
})

test_that("dpqr works for exponential", {
  # random numbers
  expect_equal({set.seed(123); rexp(10, 0.5)},
               {set.seed(123); random_numbers(ed1, 10)})
})


test_that("random numbers works for exponential", {
  # random numbers
  expect_equal({set.seed(123); rexp(10, 0.5)},
               {set.seed(123); random_numbers(ed1, 10)})
})



# Check adjusting parameters ----------------------------------------------

g1 <- make_gamma_distr()

cc <- c("sl_" = 1, "log_sl_" = 1, "sl_:forest" = 2, "log_sl_:forest" = 2)
md <- data.frame(forest = 1)

test_that("Parameter adjustment works", {
adjust_distr(g1)$params
adjust_distr(g1, shape = ~ sl_, coefs = cc, model_data = md)
adjust_distr(g1, shape = ~ sl_ + sl_:forest, coefs = cc, model_data = md)
adjust_distr(g1, shape = ~ sl_ + forest:sl_, coefs = cc, model_data = md)
expect_error(adjust_distr(g1, shape = ~ sl_ + sl_log, coefs = cc, model_data = md))

adjust_distr(g1, scale = ~ sl_, coefs = cc, model_data = md)
adjust_distr(g1, scale = ~ sl_ + sl_:forest, coefs = cc, model_data = md)
adjust_distr(g1, scale = ~ sl_ + forest:sl_, coefs = cc, model_data = md)
adjust_distr(g1, scale = ~ sl_ + sl_log, coefs = cc, model_data = md)

})


