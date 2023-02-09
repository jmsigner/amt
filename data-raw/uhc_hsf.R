## Simulating data for example UHC plots
# Simulate from HSF

# Load packages ----
library(tidyverse)
# library(raster) # Convert to terra
library(terra)

# Habitat data ----
attach("data/uhc_hab.rda")
data(uhc_hab)
hab <- rast(uhc_hab, type = "xyz", crs = "epsg:32612")
# Convert "cover" layer to factor
levels(hab[[4]]) <- data.frame(id = 1:3,
                                    cover = c("grass", "forest", "wetland"))

# Coefficients ----
# Forage is resource
beta_forage = log(5)/500

# Temperature is condition (quadratic term)
beta_temp2 = -1 * log(2)/36
beta_temp = beta_temp2 * -26

# Predator density is risk
beta_pred = log(0.25)/5

# Cover has 3 levels, forest > grassland > wetland
# Grassland is intercept
beta_forest <- log(2)
beta_wetland <- log(1/2)

# Distance to water, distance to center, and random layers DO NOT affect
# selection.

# Calculate w(x) ----
# Get our raster data into a data.frame
dat <- as.data.frame(hab, xy = TRUE) %>%
  # Calculate g(x) [linear predictor]
  mutate(g =
           # forage
           beta_forage * forage +
           # two terms for temperature
           beta_temp * temp +
           beta_temp2 * temp^2 +
           # predator density
           beta_pred * pred +
           # landcover
           beta_forest * (cover == "forest") +
           beta_wetland * (cover == "wetland")) %>%
  # Calculate w(x)
  mutate(w = exp(g))


# Draw random points ----
# How many GPS points do we want?
n_pts <- 2000

# Normalize w(x)
dat$w_prime <- dat$w/sum(dat$w)

# Expected number of points
dat$lambda <- n_pts * dat$w_prime

# Draw realized points from Poisson
set.seed(20220301 + 1)
dat$n <- rpois(n = nrow(dat), lambda = dat$lambda)

# Change random points to "GPS" locations ----
# Function to jitter data
jitter <- function(x, y, min = -25, max = 25) {
  res <- data.frame(x = x + runif(1, min, max),
                    y = y + runif(1, min, max))
  return(res)
}

# Now we split each row with n > 1 into a list element
dat_list <- dat %>%
  filter(n > 0) %>%
  split(1:nrow(.))

# Create jittered points for each element of our list.
set.seed(20220301 + 2)
gps <- lapply(dat_list, function(d) {
  replicate(d$n, jitter(x = d$x, y = d$y), simplify = FALSE) %>%
    bind_rows()
}) %>%
  bind_rows()

# Check ----
plot(hab[[1]])
points(gps$x, gps$y)

# Save ----
# Rename
uhc_hsf_locs <- gps
usethis::use_data(uhc_hsf_locs, overwrite = TRUE)
