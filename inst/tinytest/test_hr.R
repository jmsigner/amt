data(fisher)
set.seed(123)
mini_fisher <- amt_fisher[sample(nrow(amt_fisher), 150), ]


# MPC and KDE have the right class

expect_true(is(mini_fisher %>% hr_mcp, "mcp"))
expect_true(is(mini_fisher %>% hr_mcp, "hr"))
expect_true(is(mini_fisher %>% hr_kde, "kde"))
expect_true(is(mini_fisher %>% hr_kde, "hr"))
expect_true(is(mini_fisher %>% hr_locoh, "locoh"))
expect_true(is(mini_fisher %>% hr_locoh, "hr"))
expect_true(is(mini_fisher %>% hr_akde, "akde"))
expect_true(is(mini_fisher %>% hr_akde, "hr"))
