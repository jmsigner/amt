#' GPS tracks from four fishers
#'
#' This file includes spatial data from 4 fisher (Martes pennanti). These location data were collected via a 105g GPS tracking collar (manufactured by E-obs GmbH) and programmed to record the animal's location every 10 minutes, continuously. The data usage is permitted for exploratory purposes. For other purposes please get in contact (Scott LaPoint).
#'
#' @format A `tibble` with 32400 rows and 5 variables:
#' \describe{
#'   \item{id}{id of the animal}
#'   \item{x_}{the x-coordinate}
#'   \item{y_}{the y-coordinate}
#'   \item{t_}{the timestamp}
#'   \item{burst_}{bursts with 10 min sampling rates}
#' }
#' @source https://www.datarepository.movebank.org/handle/10255/move.330
#' @references For more information, contact Scott LaPoint `sdlapoint@gmail.com`
"amt_fisher"

#' Landuse for fisher data
#'
#' A Gauss-Random-Field simulation of a landscape.
#'
#' @format A `RasterLayer`
"amt_fisher_lu"
