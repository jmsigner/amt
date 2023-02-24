library(amt)

map <- terra::rast("data-raw/sh_forest.tif")

map <- terra::ifel(map == 1, 1, 0)
names(map) <- "forest"
sh_forest <- terra::wrap(map)
saveRDS(sh_forest, "inst/external/sh_forest.rds")

usethis::use_data(sh_forest, overwrite = TRUE, version = 3)
