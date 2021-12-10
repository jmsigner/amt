#' Create a template raster layer
#'
#' For some home-range estimation methods (e.g., KDE) a template raster is needed. This functions helps to quickly create such a template raster.
#' @param factor `[numeric(1)=1.5]{>= 1}`\cr Factor by which the extent of the relocations is extended.
#' @param res `[numeric(1)]`\cr Resolution of the output raster.
#' @template dots_none
#' @template track_xy_star
#' @name trast
#' @return A `RasterLayer` without values.
#' @export
make_trast <- function(x, ...) {
  UseMethod("make_trast", x)
}

#' @export
#' @rdname trast
make_trast.track_xy <- function(x, factor = 1.5, res = max(c(extent_max(x) / 100, 1e-9)), ...) {

  checkmate::assert_number(factor, lower = 1)
  checkmate::assert_number(res, lower = 1e-10)

  me <- extent_max(x)
  bu <- me * factor - me
  raster::raster(amt::bbox(x, buffer = bu), res = res)


}
