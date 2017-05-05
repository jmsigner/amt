#' Simulate UD from fitted SSF
#'
#' Function to obtain a habitat kernel from a fitted (i)SSF.
#'
#' The habitat kernel is calculated by multiplying resources with their corresponding coefficients from the fitted (i)SSF.
#'
#' @template fit
#' @template resources
#' @references
#' \insertRef{avgar2016}{amt}
#'
#' \insertRef{signer2017}{amt}
#'
#' @return The habitat kernel, as `RasterLayer`.
#' @name sim_ud

NULL

#' @param movement_kernel A `RasterLayer` providing the movement kernel.
#' @param habitat_kernel A `RasterLayer` providing the kernel kernel.
#' @param start A numeric vector of length two, giving the starting point of the simulatoin.
#' @param n An integer, providing the number of simulation steps.
#' @details  **`simulate_ud()`:** simulates a utilization distribution (UD) from a fitted (iSSF).
#' @rdname sim_ud
#' @export
simulate_ud <- function(movement_kernel, habitat_kernel, start, n = 1e5L) {
  nc <- ncol(habitat_kernel)
  nr <- nrow(habitat_kernel)

  mk <- raster::rasterToPoints(movement_kernel)
  mk[, c("x", "y")] <- mk[, c("x", "y")] / raster::res(movement_kernel)[1]

  hk1 <- as.vector(m <- t(raster::as.matrix(habitat_kernel)))
  y <- as.vector(col(m))
  x <- as.vector(row(m))
  hk1 <- cbind(cell = nc * (y - 1) + x, hk1)

  # start cell
  start1 <- nc * (nr - start[2]) + start[1]

  s <- simulate_udf(n, start1, 200, 200, mk, hk1)
  m <- raster::setValues(habitat_kernel, s)
  m <- m / sum(m[])
  m

}

#' @rdname sim_ud
#' @details  **`issf_to_ud()`:** this function is a convenience wrapper, that combines the functions `movement_kernel`, `habitat_kernel` and `simulate_ud`.
#' @export
issf_to_ud <- function(fit, resources, start, quant = 0.99, adjust = TRUE, n = 1e5) {
  mk <- movement_kernel(fit = fit, resources = resources, quant = quant, adjust = adjust)
  hk <- habitat_kernel(fit, resources, exp = TRUE)
  ud <- simulate_ud(movement_kernel = mk, habitat_kernel = hk, start = start, n = n)
  ud
}


