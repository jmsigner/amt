#' @param exp A logical scalar, indicating whether or not the resulting habitat kernel should be exponentiated. This is usually `TRUE`.
#' @param coef `[numeric]` \cr Vector with coeffiecients, not yet implemented.
#' @rdname sim_ud
#' @export
habitat_kernel <- function(fit, resources, exp = TRUE, coef = NULL) {

  vars <- intersect(
    names(resources),
    names(coef(fit)))

  if (!length(vars)) {
    stop("Coef and covars share no common arg.")
  }

  hk <- raster::raster(resources)
  hk <- raster::setValues(hk, 0)
  for (i in vars) {
    hk <- hk + resources[[i]] * coef(fit)[i]
  }
  if (exp) {
    hk <- exp(hk)
  }
  hk
}
