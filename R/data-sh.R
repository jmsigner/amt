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

#' Forest cover
#'
#' Forest cover for the home range of one red deer in northern Germany.
#'
#' @format A `RasterLAyer` \describe{ \item{1}{forest} \item{2}{non-forest} }
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
