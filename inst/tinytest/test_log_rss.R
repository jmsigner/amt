library(amt)
data(deer)
data("sh_forest")
d <- deer[1:50, ] %>% random_points() %>% extract_covariates(sh_forest) %>%
  mutate(w = ifelse(case_, 1, 1000))

f1 <- glm(case_ ~ sh.forest, data = d, family = binomial())
f2 <- fit_logit(d, case_ ~ sh.forest)

x1 <- data.frame(sh.forest = 0)
x2 <- data.frame(sh.forest = 1)

rss1 <- log_rss(f1, x1, x2)
rss2 <- log_rss(f2, x1, x2)

tinytest::expect_equal(rss1, rss2)
