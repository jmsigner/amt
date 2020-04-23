#' Simulate UD from fitted SSF
#'
#' Function to obtain a habitat kernel from a fitted (i)SSF.
#'
#' The habitat kernel is calculated by multiplying resources with their corresponding coefficients from the fitted (i)SSF.
#'
#' @param resources `[RasterLayer, RasterStack]` \cr The resources.
#' @param movement_kernel `[RasterLayer]` \cr The movement kernel.
#' @param habitat_kernel `[RasterLayer]` \cr The habitat kernel.
#' @param start `[numeric(2)]` \cr Starting point of the simulation.
#' @param n `[integer(1)=1e5]` \cr The number of simulation steps.
#' @references
#' \insertRef{avgar2016}{amt}
#' \insertRef{signer2017}{amt}
#'
#' @return The habitat kernel, as `RasterLayer`.
#' @name sim_ud
#' @note This functions are still experimental and should be used with care. If in doubt, please contact the author.
#' @author Johannes Signer (jmsigner@@gmail.com)

NULL

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

#' @param n_rep `[integer(1)=5e3]{>0}` \cr The number of times the animal walks of the final position. The mean of all replicates is returned.
#' @details  **`simulate_tud()`:** Is a convenience wrapper around `simulate_ud` to simulate transition UDs (i.e., starting at the same position many times and only simulate for a short time).
#' @rdname sim_ud
#' @export

simulate_tud <- function(movement_kernel, habitat_kernel, start, n = 100, n_rep = 5e3) {
  tud <- habitat_kernel
  tud <- raster::setValues(tud, 0)
  for(i in 1:n_rep) {
    tud <- tud +
      simulate_ud(movement_kernel,
                  habitat_kernel, start, n = n)
  }
  tud <- raster::setValues(tud, raster::getValues(tud) /
                             sum(raster::getValues(tud)))
}
