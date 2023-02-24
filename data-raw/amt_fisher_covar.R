
elevation <- terra::rast("data-raw/amt_fisher_covar_elevation.tif")
names(elevation) <- "elevation"
landuse <- terra::rast("data-raw/amt_fisher_covar_landuse.tif")
names(landuse) <- "landuse"
popden <- terra::rast("data-raw/amt_fisher_covar_popden.tif")
names(popden) <- "popden"

amt_fisher_covar <- list(
  elevation = terra::wrap(elevation),
  landuse = terra::wrap(landuse),
  popden = terra::wrap(popden)
)

saveRDS(amt_fisher_covar, "inst/external/amt_fisher_covar.rds")

usethis::use_data(amt_fisher_covar, overwrite = TRUE)
