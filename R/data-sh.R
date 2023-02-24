#' Relocations of one Red Deer
#'
#' 1500 GPS fixes of one red deer from northern Germany.
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

#' Forest cover
#'
#' Forest cover for the home range of one red deer from northern Germany.
#'
#' @format A `SpatRast` \describe{ \item{0}{other} \item{1}{forest} }
#' @source JRC
#' @references A. Pekkarinen, L. Reithmaier, P. Strobl (2007): Pan-European
#'   Forest/Non-Forest mapping with Landsat ETM+ and CORINE Land Cover 2000
#'   data.
"sh_forest"

#' Relocations of 1 red deer
#'
#' 826 GPS relocations of one red deer from northern Germany. The data is
#' already resampled to a regular time interval of 6 hours and the coordinate
#' reference system is transformed to `epsg:3035`.
#'
#' @format A track_xyt \describe{ \item{x_}{the x-coordinate} \item{y_}{the
#'   y-coordinate} \item{t_}{the timestamp} \item{burst_}{the burst a particular
#'   points belongs to.} }
#' @source Verein für Wildtierforschung Dresden und Göttingen e.V.
"deer"

#' Helper function to get forest cover
#'
#' The current version of `terra` (1.7.12) requires `SpatRast`ers to be wrapped in order to be saved locally. This function unwraps the the forest layer and returns a `SpatRast`.
#'
#' @return A `SpatRast` with forest cover.
#' @export

get_sh_forest <- function() {
  #e <- new.env()
  #x <- data("sh_forest", envir = e)[1]
  sh_forest <- readRDS(system.file("external", "sh_forest.rds", package = "amt"))
  terra::unwrap(sh_forest)
}
