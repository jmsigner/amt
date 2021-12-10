# Simulate data
library(amt)

data(deer)
expect_equivalent(from(deer), min(deer$t_))
expect_equivalent(to(deer), max(deer$t_))
expect_equivalent(from_to(deer), range(deer$t_))
