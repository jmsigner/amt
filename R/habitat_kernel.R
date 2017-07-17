#' @param exp A logical scalar, indicating whether or not the resulting habitat kernel should be exponentiated. This is usually `TRUE`.
#' @param coef A vector of coefficients for each resource.
#' @rdname sim_ud
#' @export
habitat_kernel <- function(coefs, resources, exp = TRUE) {

  hk <- raster::raster(resources)
  hk <- raster::setValues(hk, 0)
  for (i in 1:length(coefs)) {
    hk <- hk + resources[[i]] * coefs[i]
  }
  if (exp) {
    hk <- exp(hk)
  }
  hk
}
