data(deer)
d1 <- make_track(deer, x_, y_)
d2 <- deer %>%  make_track(x_, y_, t_)

expect_true(is(d1 %>% steps(), "steps_xy"))
expect_true(is(d1 %>% steps(), "data.frame"))
expect_true(is(d2 %>% steps(), "steps_xyt"))
expect_true(is(d2 %>% steps(), "steps_xy"))
expect_true(is(d2 %>% steps(), "data.frame"))
expect_true(is(d1 %>% steps() %>% random_steps(), "random_steps"))
expect_true(is(d2 %>% steps() %>% random_steps(), "random_steps"))

# Distributions

sl <- fit_distr(d2 %>% steps() %>% pull(sl_), "gamma")
expect_equal(d2 %>% steps() %>% pull(sl_) %>% fit_distr(dist_name = "gamma") %>% getElement("name"), "gamma")
expect_equal(d2 %>% steps() %>% pull(sl_) %>% fit_distr(dist_name = "unif") %>% getElement("name"), "unif")
expect_equal(d2 %>% steps() %>% pull(sl_) %>% fit_distr(dist_name = "exp") %>% getElement("name"), "exp")
