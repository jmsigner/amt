#' Simulate UD from fitted SSF
#'
#' Function to obtain a habitat kernel from a fitted (i)SSF.
#'
#' The habitat kernel is calculated by multiplying resources with their corresponding coefficients from the fitted (i)SSF.
#'
#' @param resources `[RasterLayer, RasterStack]` \cr The resources.
#' @references
#' \insertRef{avgar2016}{amt}
#' \insertRef{signer2017}{amt}
#'
#' @return The habitat kernel, as `RasterLayer`.
#' @name sim_ud
#' @note This functions are still experimental and should be used with care. If in doubt, please contact the author.
#' @author Johannes Signer (jmsigner@@gmail.com)

NULL

#' @param movement_kernel `[RasterLayer]` \cr The movement kernel.
#' @param habitat_kernel `[RasterLayer]` \cr The habitat kernel.
#' @param start `[numeric(2)]` \cr Starting point of the simulation.
#' @param n `[integer(1)=1e5]` \cr The number of simulation steps.
#' @details  **`simulate_ud()`:** simulates a utilization distribution (UD) from a fitted Step-Selection Function.
#' @rdname sim_ud
#' @export
simulate_ud <- function(movement_kernel, habitat_kernel, start, n = 1e5L) {

  # Get extent of the raster
  nc <- ncol(habitat_kernel)
  nr <- nrow(habitat_kernel)

  # normalize movement kernel (i.e., remove coordinates)
  mk <- raster::rasterToPoints(movement_kernel)
  mk[, c("x", "y")] <- mk[, c("x", "y")] / raster::res(movement_kernel)[1]

  # convert habitat kernel to row-major order
  # cont. here
  # index = X + Y * Width;
  # Y = (int)(index / Width)
  # X = index - (Y * Width)
  hk1 <- as.vector(m <- t(raster::as.matrix(habitat_kernel)))
  x <- as.vector(col(m))
  y <- as.vector(row(m))
  hk1 <- cbind(cell = nc * (y - 1) + x, hk1)

  # start cell
  start1 <- raster::cellFromXY(habitat_kernel, start)
  # start <- nc * (nr - start[2]) + start[1]

  s <- simulate_udf(n, start1, nc, nr, mk, hk1)
  m <- raster::setValues(habitat_kernel, s)
  m <- m / sum(m[])
  m

}

