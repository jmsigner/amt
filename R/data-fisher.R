#' Relocations of 1 red deer
#'
#' 1500 GPS relocations of one red deer from northern Germany.
#'
#' @format A data frame with 1500 rows and 4 variables:
#' \describe{
#'   \item{x_epsg31467}{the x-coordinate}
#'   \item{y_epsg31467}{the y-coordinate}
#'   \item{day}{the day of the relocation}
#'   \item{time}{the hour of the relocation}
#' }
#' @source Verein für Wildtierforschung Dresden und Göttingen e.V.
"sh"

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
#' @format A list with three where each entry is a `RasterLayer`.
"amt_fisher_covar"
