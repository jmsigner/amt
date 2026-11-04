library(amt)

# Renaming a stratum or the response must not change estimates or intervals.
set.seed(101)
n_strata <- 80L
ssf <- data.frame(
  used = rep(c(TRUE, FALSE, FALSE), n_strata),
  step_id_ = rep(seq_len(n_strata), each = 3L),
  x = rnorm(3L * n_strata),
  habitat = factor(sample(c("forest", "open"), 3L * n_strata, replace = TRUE))
)
ssf$step_id <- paste0("individual=1_step=", ssf$step_id_)
ssf$case_ <- ssf$used
model_default <- fit_clogit(ssf, case_ ~ x * habitat + strata(step_id_), model = TRUE)
model_named <- fit_clogit(ssf, used ~ x * habitat + strata(step_id), model = TRUE)
x1 <- data.frame(x = c(-1, 0.5, 1),
                 habitat = factor(c("forest", "open", "forest"), levels = levels(ssf$habitat)))
x2 <- data.frame(x = 0, habitat = factor("forest", levels = levels(ssf$habitat)))

rss_default <- log_rss(model_default, x1, x2, ci = "se")$df
rss_named <- log_rss(model_named, x1, x2, ci = "se")$df
expect_equal(rss_named, rss_default, tolerance = 1e-10)

# Independently calculate the contrast and its variance from the model matrix.
mm1 <- model.matrix(~ x * habitat, x1)[, -1, drop = FALSE]
mm2 <- model.matrix(~ x * habitat, x2)[, -1, drop = FALSE]
delta <- sweep(mm1, 2, mm2[1, ])
estimate <- unname(drop(delta %*% coef(model_named$model)))
se <- unname(sqrt(diag(delta %*% vcov(model_named$model) %*% t(delta))))
expect_equal(rss_named$log_rss, estimate, tolerance = 1e-10)
expect_equal(rss_named$lwr, estimate - qnorm(.975) * se, tolerance = 1e-10)
expect_equal(rss_named$upr, estimate + qnorm(.975) * se, tolerance = 1e-10)
expect_equal(log_rss(model_named, x1, x2)$df$log_rss, estimate, tolerance = 1e-10)

# Compare the contrast with survival's predictions.
pred1 <- predict(model_default$model, transform(x1, step_id_ = ssf$step_id_[1]),
                 type = "lp", reference = "sample")
pred2 <- predict(model_default$model, transform(x2, step_id_ = ssf$step_id_[1]),
                 type = "lp", reference = "sample")
expect_equal(rss_default$log_rss, unname(pred1 - pred2[1]), tolerance = 1e-10)

# Bootstrap uses complete strata and works for custom names and responses.
set.seed(202)
boot_default <- log_rss(model_default, x1, x2, ci = "boot", n_boot = 20)$df
set.seed(202)
boot_named <- log_rss(model_named, x1, x2, ci = "boot", n_boot = 20)$df
expect_equal(boot_named, boot_default, tolerance = 1e-8)
expect_true(all(is.finite(boot_named$lwr)))
expect_true(all(is.finite(boot_named$upr)))

# Compare one seeded bootstrap replicate with an independent refit that
# explicitly assigns a distinct stratum ID to each draw.
set.seed(212)
draws <- sample.int(n_strata, n_strata, replace = TRUE)
manual_data <- do.call(rbind, lapply(seq_along(draws), function(i) {
  out <- ssf[ssf$step_id_ == draws[i], ]
  out$bootstrap_id <- i
  out
}))
manual_fit <- fit_clogit(manual_data, used ~ x * habitat + strata(bootstrap_id), model = TRUE)
manual_estimate <- unname(drop(delta %*% coef(manual_fit$model)))
set.seed(212)
expect_equal(amt:::boot1.fit_clogit(model_named, x1, x2), manual_estimate, tolerance = 1e-8)

set.seed(303)
boot_single <- log_rss(model_named, x1[1, , drop = FALSE], x2, ci = "boot", n_boot = 1)$df
expect_equal(nrow(boot_single), 1L)
expect_true(is.finite(boot_single$lwr))

# Offset contrasts must be retained in the estimates and bootstrap.
ssf$known_offset <- seq(-0.5, 0.5, length.out = nrow(ssf))
offset_model <- fit_clogit(ssf, used ~ x + offset(known_offset) + strata(step_id), model = TRUE)
offset_x1 <- data.frame(x = c(1, 2), known_offset = c(0.4, -0.2))
offset_x2 <- data.frame(x = 0, known_offset = 0.1)
offset_expected <- coef(offset_model$model)[["x"]] * offset_x1$x + offset_x1$known_offset - 0.1
expect_equal(log_rss(offset_model, offset_x1, offset_x2)$df$log_rss, offset_expected, tolerance = 1e-10)
set.seed(404)
offset_boot <- log_rss(offset_model, offset_x1, offset_x2, ci = "boot", n_boot = 10)$df
expect_true(all(is.finite(offset_boot$lwr)))

# Retain offsets and transformed predictors when their order changes.
mixed_model <- fit_clogit(ssf, used ~ offset(known_offset) + x + I(x^2) + habitat +
                           strata(step_id), model = TRUE)
mixed_x1 <- data.frame(x = c(-1, 0.5), known_offset = c(0.2, -0.1),
                       habitat = factor(c("forest", "open"), levels = levels(ssf$habitat)))
mixed_x2 <- data.frame(x = 0, known_offset = 0.1,
                       habitat = factor("forest", levels = levels(ssf$habitat)))
mixed_pred1 <- predict(mixed_model$model, transform(mixed_x1, step_id = ssf$step_id[1]),
                       type = "lp", reference = "sample")
mixed_pred2 <- predict(mixed_model$model, transform(mixed_x2, step_id = ssf$step_id[1]),
                       type = "lp", reference = "sample")
expect_equal(log_rss(mixed_model, mixed_x1, mixed_x2, ci = "se")$df$log_rss,
             unname(mixed_pred1 - mixed_pred2[1]), tolerance = 1e-10)

# A stratum expression combining multiple variables does not need those
# variables in x1 or x2.
ssf$block <- rep(rep(c("A", "B"), length.out = n_strata), each = 3L)
combined_model <- fit_clogit(ssf, used ~ x + strata(step_id, block), model = TRUE)
combined_x1 <- data.frame(x = c(-1, 1))
combined_x2 <- data.frame(x = 0)
expect_equal(log_rss(combined_model, combined_x1, combined_x2)$df$log_rss,
             unname(coef(combined_model$model)[["x"]] * combined_x1$x), tolerance = 1e-10)
set.seed(505)
expect_true(all(is.finite(log_rss(combined_model, combined_x1, combined_x2,
                                  ci = "boot", n_boot = 5)$df$lwr)))

# Two strata terms give the same results as their combined form.
separate_model <- fit_clogit(ssf, used ~ x + strata(step_id) + strata(block), model = TRUE)
expect_equal(log_rss(separate_model, combined_x1, combined_x2)$df$log_rss,
             log_rss(combined_model, combined_x1, combined_x2)$df$log_rss,
             tolerance = 1e-10)
set.seed(606)
separate_boot <- log_rss(separate_model, combined_x1, combined_x2, ci = "boot", n_boot = 5)$df
set.seed(606)
combined_boot <- log_rss(combined_model, combined_x1, combined_x2, ci = "boot", n_boot = 5)$df
expect_equal(separate_boot, combined_boot, tolerance = 1e-8)
