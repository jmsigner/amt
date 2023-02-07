## Simulating data for example UHC plots
# Habitat rasters

# Load packages ----
library(tidyverse)
# library(raster) # Convert to terra
library(terra)
library(mvtnorm)

# Coordinates and distance matrix ----
coords <- expand.grid("x" = seq(-2000, 1950, by = 50),
                      "y" = seq(-2000, 1950, by = 50)) %>%
  arrange(x, y) %>%
  as.matrix()

dm <- as.matrix(dist(coords))
dm <- dm/max(dm)

# Covariance matrix ----
# Exponential covariance function
expcov <- function(dists, rho, sigma) {
  n <- dim(dists)[1]
  result <- matrix(nrow = n, ncol = n)
  sigma2 <- sigma^2
  result <- sigma2 * exp(-1 * dists/rho)
  return(result)
}

# Generate habitats ----

# ... forage ----
# units are g/m2
# reasonable values are 0 - 1000
# make it zero-inflated (rocky outcrops?)

set.seed(20220105 + 1)
forage <- rmvnorm(n = 1,
                  mean = rep(4, nrow(coords)),
                  sigma = expcov(dm, 3, 2))
# Stretch to range (create some negatives)
forage <- (forage - (min(forage) * 1.08)) / max(forage - (min(forage) * 1.08)) * 1000

# Cut any negative values to 0
forage[forage < 0] <- 0

# Check
hist(forage, breaks = 30)

# ... mean annual temp ----
# units are degrees C
# reasonable values are 0 - 20 deg C
set.seed(20220105 + 2)
temp <- rmvnorm(n = 1,
                mean = rep(13, nrow(coords)),
                sigma = expcov(dm, 1, 5))

# Check
hist(temp, breaks = 30)

# ... predation risk ----
# units are predators/100 km2
# reasonable values are 0 - 15 (think large predator like wolf)
# want mean to vary through space

# Create mean with sine waves in x and y coords
sin_x <- sin(((coords[, "x"]/2000) * 2 * pi) * 1.5)
sin_y <- sin(((coords[, "y"]/2000) * 2 * pi) * 1.5)
mu_pred <- sin_x + sin_y + sin_x * sin_y

# TT <- as.data.frame(coords) %>%
#   mutate(pred = mu_pred) %>%
#   rasterFromXYZ(res = 50, crs = 32612)
# plot(TT)

set.seed(20220105 + 3)
pred <- rmvnorm(n = 1,
                mean = ((mu_pred + 1.75) * 3) + 4,
                sigma = expcov(dm, 0.75, 3.5))
pred[pred < 0] <- 0

# Check
hist(pred, breaks = 30)

# ... landcover ----
# categories are:
#   - grassland (50%)
#   - forest (30%)
#   - wetland (20%)
# generate some random centroids, then make patches with closest centroid

# Cell numbers as potential centroids
set.seed(20220105 + 4)
cells <- sample.int(n = nrow(coords), size = 0.2 * nrow(coords), replace = FALSE)

# Centroids of grassland
g_cent <- sample(cells, size = 0.5 * length(cells), replace = FALSE)
remain <- cells[which(!cells %in% g_cent)]

# Centroids of forest
f_cent <- sample(remain, size = 0.3 * length(cells), replace = FALSE)

# Centroids of wetland
w_cent <- remain[which(!remain %in% f_cent)]

# Place in cells
coord_df <- as.data.frame(coords)
coord_df$grass <- coord_df$forest <- coord_df$wet <- NA
coord_df$grass[g_cent] <- 1
coord_df$forest[f_cent] <- 1
coord_df$wet[w_cent] <- 1

# Rasterize
cent <- rast(coord_df, type = "xyz", crs = "epsg:32612")

# Distance to each
dist_grass <- distance(cent$grass)
dist_forest <- distance(cent$forest)
dist_wet <- distance(cent$wet)
dist_stack <- c(dist_grass, dist_forest, dist_wet)
names(dist_stack) <- c("grass", "forest", "wet")

# Pick the type with smallest distance
cover_df <- as.data.frame(dist_stack, xy = TRUE) %>%
  pivot_longer(cols = grass:wet, names_to = "cover", values_to = "dist") %>%
  # Jitter distances a tiny bit
  mutate(dist = dist + rnorm(n = nrow(.))) %>%
  group_by(x, y) %>%
  filter(dist == min(dist)) %>%
  # Convert back to numeric values
  mutate(cover_num = case_when(
    cover == "grass" ~ 1,
    cover == "forest" ~ 2,
    cover == "wet" ~ 3
  ))

# Check
table(cover_df$cover_num)/nrow(coords)

# Rasterize ----
# Compile in data.frame
g_dat <- data.frame(x = coords[, 1] + 447000,
                    y = coords[, 2] + 4626000,
                    forage = forage[1, ],
                    temp = temp[1, ],
                    pred = pred[1, ],
                    cover = cover_df$cover_num)

# Rasterize
rast <- rast(g_dat, type = "xyz", crs = "epsg:32612")
template <- rast[[1]]
names(template) <- NA

# Additional covariates ----

# ... distance to water ----
h2o <- rast[[4]] == 3
h2o[h2o == 0] <- NA
dist_to_water <- distance(h2o)
names(dist_to_water) <- "dist_to_water"

# ... distance to center ----
cent <- template
cent[] <- NA
cent[cellFromXY(cent, cbind(mean(g_dat$x), mean(g_dat$y)))] <- 1

dist_to_cent <- distance(cent)

# ... random ----
set.seed(20220105 + 5)
rand <- template
rand[] <- round(rnorm(ncell(rand)))

# Final stack ----
uhc_rast <- c(rast, dist_to_water, dist_to_cent, rand)
names(uhc_rast)[5:7] <- c("dist_to_water", "dist_to_cent", "rand")

# Save ----
# The terra package doesn't work properly (due to broken C++ pointers) when
# saving to *.rda. Instead, need to save the values.
uhc_hab <- as.data.frame(uhc_rast, xy = TRUE)
usethis::use_data(uhc_hab, overwrite = TRUE)
