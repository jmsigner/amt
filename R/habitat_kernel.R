#' @param exp A logical scalar, indicating whether or not the resulting habitat kernel should be exponentiated. This is usually `TRUE`.
#' @param coef `[list]` \cr Vector with coeffiecients, not yet implemented.
#' @rdname sim_ud
#' @export
habitat_kernel <- function(coef, resources, exp = TRUE) {

  if (!all(names(coef) %in% names(resources))) {
    stop("Coef and covars share no common arg.")
  }

  hk <- raster::raster(resources)
  hk <- raster::setValues(hk, 0)

  for (i in names(coef)) {
    hk <- hk + resources[[i]] * coef[[i]]
  }
  if (exp) {
    hk <- exp(hk)
  }
  hk
}
