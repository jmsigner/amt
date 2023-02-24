library(amt)
data(deer)
sh_forest <- get_sh_forest()
d <- deer[1:50, ] |> random_points() |> extract_covariates(sh_forest) |>
  mutate(w = ifelse(case_, 1, 1000))

f1 <- glm(case_ ~ forest, data = d, family = binomial())
f2 <- fit_logit(d, case_ ~ forest)

x1 <- data.frame(forest = 0)
x2 <- data.frame(forest = 1)

rss1 <- log_rss(f1, x1, x2)
rss2 <- log_rss(f2, x1, x2)

expect_equal(rss1, rss2)

# Test `check_factors`

data("amt_fisher")
amt_fisher_covar <- get_amt_fisher_covars()

# Prepare data for RSF
rsf_data <- amt_fisher |>
  filter(name == "Lupe") |>
  dplyr::slice_sample(n = 250) |>
  make_track(x_, y_, t_) |>
  random_points() |>
  extract_covariates(amt_fisher_covar$elevation) |>
  extract_covariates(amt_fisher_covar$popden) |>
  extract_covariates(amt_fisher_covar$landuse) |>
  mutate(lu = factor(landuse))

# Fit RSF without factor
m1 <- rsf_data |>
  fit_rsf(case_ ~ elevation + popden)

# data.frame of x1s
x1 <- data.frame(elevation = seq(90, 120, length.out = 100),
                 popden = mean(rsf_data$popden))
# data.frame of x2 (note factor levels should be same as model data)
x2 <- data.frame(elevation = mean(rsf_data$elevation),
                 popden = mean(rsf_data$popden))

# Function should return NULL (no factors to check)
expect_null(amt:::check_factors(m1$model, x1, x2))

# Fit RSF with factor
m2 <- rsf_data |>
  fit_rsf(case_ ~ lu + elevation + popden)

# data.frame of x1s
x1 <- data.frame(lu = factor(50, levels = levels(rsf_data$lu)),
                 elevation = seq(90, 120, length.out = 100),
                 popden = mean(rsf_data$popden))
# data.frame of x2 (note factor levels should be same as model data)
x2 <- data.frame(lu = factor(50, levels = levels(rsf_data$lu)),
                 elevation = mean(rsf_data$elevation),
                 popden = mean(rsf_data$popden))

# Function should return NULL (no discrepancies)
expect_null(amt:::check_factors(m2, x1, x2))

# Now misspecify factor for x1
x1 <- data.frame(lu = factor(50),
                 elevation = seq(90, 120, length.out = 100),
                 popden = mean(rsf_data$popden))

# Function should return informative errors regarding data vs x1 and x1 vs x2
expect_error(amt:::check_factors(m2$model, x1, x2))

# Also misspecify factor for x2
x2 <- data.frame(lu = factor(50),
                 elevation = mean(rsf_data$elevation),
                 popden = mean(rsf_data$popden))

# Function should return informative errors regarding data vs x1 and data vs x2
expect_error(amt:::check_factors(m2$model, x1, x2))
