#' GPS tracks from four fishers
#'
#' This file includes spatial data from 4 fisher (**Pekania pennanti**). These location data were collected via a 105g GPS tracking collar (manufactured by E-obs GmbH) and programmed to record the animal's location every 10 minutes, continuously. The data re projected in NAD84 (epsg: 5070). The data usage is permitted for exploratory purposes. For other purposes please get in contact (Scott LaPoint).
#'
#' @format A `tibble` with 14230 rows and 5 variables:
#' \describe{
#'   \item{x_}{the x-coordinate}
#'   \item{y_}{the y-coordinate}
#'   \item{t_}{the timestamp}
#'   \item{sex}{the sex of the animal}
#'   \item{id}{the id of the animal}
#'   \item{name}{the name of the animal}
#' }
#' @source https://www.datarepository.movebank.org/handle/10255/move.330
#' @references For more information, contact Scott LaPoint `sdlapoint@gmail.com`
"amt_fisher"

#' Environmental data for fishers
#'
#' A list with three entries that correspond to the following three layer: land
#' use, elevation and population density.
#'
#' @source `https://lpdaac.usgs.gov/dataset_discovery/aster/aster_products_table`
#' @source `http://dup.esrin.esa.it/page_globcover.php`
#' @source `http://sedac.ciesin.columbia.edu/data/collection/gpw-v3/sets/browse`
#' @format A list with three where each entry is a `SpatRast`.
"amt_fisher_covar"

#' Helper function to get fisher covars
#'
#' The current version of `terra` (1.7.12) requires `SpatRast`ers to be wrapped in order to be saved locally. This function unwraps the covariates for the fisher data and returns a list.
#'
#' @return A list with covariates
#' @export

get_amt_fisher_covars <- function() {
 amt_fisher_covar <- readRDS(system.file("external", "amt_fisher_covar.rds", package = "amt"))
 lapply(amt_fisher_covar, terra::unwrap)
}
