#' Simulated habitat rasters for demonstrating UHC plots
#'
#' @format A `RasterStack` with 1600 cells and 7 variables:
#' \describe{
#'   \item{forage}{Forage biomass in g/m^2^ (resource)}
#'   \item{temp}{mean annual temperature in °C (condition)}
#'   \item{pred}{predator density in predators/100 km^2^ (risk)}
#'   \item{cover}{landcover (forest > grassland > wetland)}
#'   \item{dist_to_water}{distance to the wetland landcover (no effect)}
#'   \item{dist_to_cent}{distance to the centroid of the raster (no effect)}
#'   \item{rand}{random integers (no effect)}
#' }
"uhc_hab"

#' Simulated HSF location data for demonstrating UHC plots
#'
#' @format A `data.frame` with 2000 rows and 2 variables:
#' \describe{
#'   \item{x}{x-coordinate in UTM Zone 12 (EPSG: 32612)}
#'   \item{y}{Y-coordinate in UTM Zone 12 (EPSG: 32612)}
#' }
#'
#' These data were simulated assuming an ordinary habitat selection function
#' (HSF), i.e., all points are independent rather than arising from an
#' underlying movement model.
#'
#' True parameter values are:
#' - `forage` = log(5)/500 (resource)
#' - `temp^2` = -1 * log(2)/36 (condition; quadratic term)
#' - `temp` = (log(2)/36) * 26 (condition; linear term)
#' - `pred` = log(0.25)/5 (risk)
#' - `cover == "forest"` = log(2) (grassland is intercept)
#' - `cover == "wetland"` = log(1/2) (grassland is intercept)
#'
#' Note: `temp` is modeled as a quadratic term, with the strongest selection
#' occurring at 13 °C and all other temperatures less selected.
#'
#' Note: `dist_to_water`, `dist_to_cent`, and `rand` have no real effect
#' on our animal's selection and are included for demonstration purposes.
"uhc_hsf_locs"
