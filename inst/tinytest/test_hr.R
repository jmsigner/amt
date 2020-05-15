library(amt)
data(fisher)
set.seed(123)
mini_fisher <- amt_fisher[sample(nrow(amt_fisher), 150), ]


# MCP and KDE have the right class

expect_true(is(mini_fisher %>% hr_mcp, "mcp"))
expect_true(is(mini_fisher %>% hr_mcp, "hr"))
expect_true(is(mini_fisher %>% hr_kde, "kde"))
expect_true(is(mini_fisher %>% hr_kde, "hr"))
expect_true(is(mini_fisher %>% hr_locoh, "locoh"))
expect_true(is(mini_fisher %>% hr_locoh, "hr"))
expect_true(is(mini_fisher %>% hr_akde, "akde"))
expect_true(is(mini_fisher %>% hr_akde, "hr"))

# More tests for mcp
tinytest::expect_error(hr_mcp(mini_fisher, keep.data = c(TRUE, FALSE)))
tinytest::expect_error(hr_mcp(mini_fisher, keep.data = mini_fisher))
tinytest::expect_true(is(hr_mcp(mini_fisher, keep.data = TRUE)$data, "track_xy"))
tinytest::expect_true(is.null(hr_mcp(mini_fisher, keep.data = FALSE)$data))

