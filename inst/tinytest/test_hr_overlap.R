library(amt)
data(amt_fisher)
set.seed(123)

tr <- make_trast(amt_fisher[1:50, ], res = 5)

mini_fisher <- amt_fisher[1:40, ]
mcp <- hr_mcp(mini_fisher)
loc <- hr_locoh(mini_fisher)
kde <- hr_kde(mini_fisher)

mini_fisher1 <- amt_fisher[11:50, ]
mcp1 <- hr_mcp(mini_fisher1)
loc1 <- hr_locoh(mini_fisher1)
kde1 <- hr_kde(mini_fisher1, tr = tr)

# MCP and KDE have the right class

expect_inherits(hr_overlap(mcp, mcp1), "tbl_df")
expect_inherits(hr_overlap(list(mcp, mcp1)), "tbl_df")

expect_inherits(hr_overlap(kde, kde1), "tbl_df")
expect_inherits(hr_overlap(list(kde, kde1)), "tbl_df")

expect_inherits(hr_overlap(kde, kde1, type = "vi"), "tbl_df")
expect_inherits(hr_overlap(list(kde, kde1), type = "vi"), "tbl_df")

