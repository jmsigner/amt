## Simulating data for example UHC plots
# Simulate from iSSF

# Load packages ----
library(tidyverse)
library(raster)
library(circular)
library(lubridate)

# Habitat data ----
attach("data/uhc_hab.rda")
hab <- uhc_hab

# Coefficients ----
# ... movement-free habitat selection ----
# Avoidance of distance from center to keep track away from boundary
beta_dist_cent = -1 * log(10)/500

# Forage is resource
beta_forage = log(8)/500

# Temperature is condition (quadratic term)
beta_temp2 = -1 * log(8)/36
beta_temp = beta_temp2 * -26

# Predator density is risk
beta_pred = log(0.20)/5

# Cover has 3 levels, forest > grassland > wetland
# Grassland is intercept
beta_forest <- log(2)
beta_wetland <- log(1/2)

# Distance to water, distance to center, and random layers DO NOT affect
# selection.

# ... selection-free movement ----
# Step-length distribution
shp <- 5
scl <- 75

# Plot
data.frame(x = seq(1, 2000, length.out = 100)) %>%
  mutate(y = dgamma(x, shape = shp, scale = scl)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  theme_bw()

# Turn-angle distribution
k <- 0.5
data.frame(x = seq(-pi, pi, length.out = 100)) %>%
  # circular::dvonmises not vectorized
  rowwise() %>%
  mutate(y = circular::dvonmises(x, mu = 0, kappa = k)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  coord_cartesian(ylim = c(0, NA)) +
  scale_x_continuous(breaks = c(-pi, -pi/2, 0, pi/2, pi),
                     labels = expression(-pi, -pi/2, 0, pi/2, pi)) +
  theme_bw()

# Setup simulation ----
# Start with data.frame of times
dat <- data.frame(time = seq(ymd_hms("2021-02-15 0:00:00", tz = "US/Mountain"),
                             ymd_hms("2021-05-15 00:00:00", tz = "US/Mountain"),
                             by = "1 hour"),
                  x = NA,
                  y = NA)

# We'll start our animal in the middle of our map
dat$x <- mean(c(extent(hab)@xmin, extent(hab)@xmax))
dat$y <- mean(c(extent(hab)@ymin, extent(hab)@ymax))

# Convert to steps
dat <- dat %>%
  # Rearrange/rename columns
  dplyr::select(x1 = x, y1 = y, t1 = time) %>%
  # Convert to steps
  mutate(x2 = lead(x1),
         y2 = lead(y1),
         t2 = lead(t1)) %>%
  filter(!is.na(t2))

# Let's define our first step's endpoint as being 50m directly
# north to get us started moving. This is also the second step's start point.
dat$y2[1] <- dat$y1[2] <- dat$y1[1] + 50

# Lastly, let's add the absolute angle of the first step
dat$abs_angle <- NA
dat$abs_angle[1] <- 0 # directly north

# Simulate movement ----
# Function to jitter data
jitter <- function(x, y, min = -25, max = 25) {
  res <- data.frame(x = x + runif(1, min, max),
                    y = y + runif(1, min, max))
  return(res)
}

# Function to crop raster to local coordinates
#   r = raster to crop
#   cent = vector of length 2 with xy-coordinates of centroid
#   d = distance (radius) of crop
crop_raster <- function(r, cent, d) {
  ext <- extent(cent[1] - d, cent[1] + d, cent[2] - d, cent[2] + d)
  res <- crop(r, ext)
  return(res)
}

# Maximum distance to calculate
qgamma(0.999, shape = shp, scale = scl)
max_d <- 1100
pgamma(max_d, shape = shp, scale = scl)

# Loop over steps ----

# We already have the first step. We need to simulate the rest.
set.seed(20220307)

for (i in 2:nrow(dat)) {
  # Report status
  cat("\nStep", i, "of", nrow(dat))

  ## Calculate selection-free movement kernel
  # Start point
  start <- cbind(dat$x1[i], dat$y1[i])
  # Crop raster
  cropped <- crop_raster(hab, start, max_d)
  # Get coordinates of cropped raster
  coords <- coordinates(cropped)
  # Distances along x and y to every cell
  dx <- coords[, 1] - start[, 1]
  dy <- coords[, 2] - start[, 2]
  # Distance to every cell
  dists <- sqrt(dx^2 + dy^2)
  # Truncate at max distance
  trunc <- which(dists > max_d)
  dists[trunc] <- NA
  # Absolute angle to every cell
  abs <- (pi/2 - atan2(dy, dx)) %% (2*pi)
  # Relative angle difference
  rel_diff <- (abs - dat$abs_angle[i-1])
  # Relative angle
  rel_angle <- ifelse(rel_diff > pi, rel_diff - 2*pi, rel_diff)
  # Likelihood of step length
  sl_like <- dgamma(dists, shape = shp, scale = scl) / (2 * pi * dists)
  # Necessary?
  sl_like <- sl_like/sum(sl_like, na.rm = TRUE)
  # Likelihood of turn angle (not vectorized -- need loop)
  ta_like <- rep(NA, length(sl_like))
  for (j in 1:length(ta_like)) {
    suppressWarnings({
      ta_like[j] <- dvonmises(rel_angle[j],
                              mu = 0,
                              kappa = k)

    })
  }
  ta_like[trunc] <- NA
  # Necessary?
  ta_like <- ta_like/sum(ta_like, na.rm = TRUE)

  # Calculate kernel values
  move_kern_vals <- sl_like * ta_like
  # Normalize (sum to 1)
  move_kern_vals <- move_kern_vals/sum(move_kern_vals, na.rm = TRUE)

  # # If you want to plot this
  # move_kern <- cropped[[1]]
  # values(move_kern) <- move_kern_vals
  # plot(move_kern, main = "Movement Kernel")

  ## Movement-free habitat kernel
  hab_kern_vals <- as.data.frame(cropped, xy = TRUE) %>%
    mutate(w = exp(
      beta_dist_cent * dist_to_cent +
        beta_forage * forage +
        beta_temp * temp +
        beta_temp2 * temp^2 +
        beta_pred * pred +
        beta_forest * (cover_VALUE == "forest") +
        beta_wetland * (cover_VALUE == "grassland")
    )) %>%
    # Normalize
    mutate(w_prime = w/sum(w)) %>%
    pull(w_prime)

  # # If you want to plot this
  # hab_kern <- cropped[[1]]
  # values(hab_kern) <- hab_kern_vals
  # plot(hab_kern, main = "Habitat Kernel")

  ## Combine
  step_kern_vals <- move_kern_vals * hab_kern_vals
  # Normalize
  step_kern_vals <- step_kern_vals/sum(step_kern_vals, na.rm = TRUE)

  # # If you want to visualize
  # step_kern <- cropped[[1]]
  # values(step_kern) <- step_kern_vals
  # plot(step_kern, main = "Habitat x Movement Kernel")

  # Randomly select cell to move into based on the probabilities
  cells <- 1:ncell(cropped)
  cells[trunc] <- NA
  next_cell <- sample(x = na.omit(cells),
                      size = 1,
                      prob = na.omit(step_kern_vals))

  # Get cell coordinates
  next_cell_coords <- xyFromCell(cropped, next_cell)

  # If you want to plot
  # points(next_cell_coords[,"x"], next_cell_coords[,"y"], pch = 16)

  # Jitter
  next_coords <- jitter(next_cell_coords[, 1], next_cell_coords[, 2])

  # Insert into data.frame
  if (i != nrow(dat)) {
    dat$x2[i] <- dat$x1[i+1] <- next_coords[, 1]
    dat$y2[i] <- dat$y1[i+1] <- next_coords[, 2]
  } else {
    dat$x2[i] <- next_coords[, 1]
    dat$y2[i] <- next_coords[, 2]
  }

  # Calculate absolute angle
  dx <- dat$x2[i] - dat$x1[i]
  dy <- dat$y2[i] - dat$y1[i]
  dat$abs_angle[i] = (pi/2 - atan2(dy, dx)) %% (2*pi)

  # If you want to check
  # dat[i, ]

}

# Check ----
plot(hab[[1]])
points(dat$x1, dat$y1)
lines(dat$x1, dat$y1)

# Convert to GPS style ----
gps <- dat %>%
  dplyr::select(x = x1, y = y1, t = t1)

# Save ----
# Rename
uhc_issf_locs <- gps
usethis::use_data(uhc_issf_locs, overwrite = TRUE)
