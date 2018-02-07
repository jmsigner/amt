#' @param scale,shape `[numeric](1)`\cr Scale and scale parameter of the gamma distribution of step lengths.
#' @param quant A numeric scalar, quantile of the step-length distribution that is
#'   the maximum movement distance.
#' @param template `[RasterLayer,RasterStack]`\cr A raster serving as template for the simulations.
#' @details  **`movement_kernel()`:** calculates a movement kernel from a fitted
#'   (i)SSF. The method is currently only implemented for the gamma
#'   distribution.
#' @export
#' @rdname sim_ud
movement_kernel <- function(scale, shape, template, quant = 0.99) {
    dist <- ceiling(qgamma(quant, scale = scale, shape = shape))
    mk <- mk_base(dist, raster::res(template)[1])
    mk <- mk_gamma(mk, shape= shape, scale = scale)
    attributes(mk)$abt <- list(scale = scale, shape = shape,
                               dist = dist,
                               quant = quant)
    mk
}

mk_base <- function(r, res) {
  xy <- c(0, rep(seq(res, r, res), each = 2) * c(-1, 1))
  mov_kern <- expand.grid(x = xy,
                          y = xy)
  mov_kern$d <- sqrt(mov_kern[, "x"]^2 + mov_kern[, "y"]^2)

  # Tal: Email 2016-02-18:  In fact, the division by 2*pi*r is a correction to the 'habitat-independent' movement kernel so
  # it should be applied to the gamma pdf values directly; the 'attractiveness' (non-normalized kernel value) of a given
  # cell positioned at distance r from the center of the kernel is the product of the gamma pdf value at r,
  # divided by 2*pi*r, and the resource selection function (exp[beta*X....]). The issue with the delta function
  # arises when r tends to 0. My workaround was to set any r < 1/(2*pi) to 1/(2*pi).

  #mov_kern$d[mov_kern$d < 1 / (2 * pi)] <- 1/(2 * pi) # why 2 * pi?
  mov_kern$d[mov_kern$d < 1 ] <- 1 # why 2 * pi?
  mov_kern <- mov_kern[mov_kern$d < r, ]

  raster::rasterFromXYZ(mov_kern, res = res)

}

mk_gamma <- function(mk, shape, scale) {
  d <- raster::getValues(mk)
  raster::setValues(mk, dgamma(d,  shape = shape, scale = scale) / (2 * pi * d))
}

