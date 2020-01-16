#' @rdname hr
#' @export
#' @references C. H. Fleming, W. F. Fagan, T. Mueller, K. A. Olson, P. Leimgruber, J. M. Calabrese, “Rigorous home-range estimation with movement data: A new autocorrelated kernel-density estimator”, Ecology, 96:5, 1182-1188 (2015).
#' @examples
#' # akde
#' \dontrun{
#' data(deer)
#' ud1 <- hr_akde(deer) # uses an iid ctmm
#' ud2 <- hr_akde(deer, model = fit_ctmm(deer, "ou")) # uses an OU ctmm
#' }
hr_akde <- function(x, ...) {
  UseMethod("hr_akde", x)
}


#' @export
#' @param model A continous time movement model. This can be fitted either with `ctmm::ctmm.fit` or `fit_ctmm`.
#' @rdname hr
hr_akde.track_xyt <- function(x, model = fit_ctmm(x, "iid"),
                              trast = make_trast(x), ...) {

  if (grepl("bm", tolower(summary(model)$name))) {
    warning("Brownian motion was chosen as movement model, akde won't work")
  }

  suppressMessages(suppressWarnings(dat <- as_telemetry(x)))
  ud <- ctmm::akde(dat, model)

  r <- 1 - ctmm::raster(ud, DF = "CDF")
  r <- raster::projectRaster(r, to = trast)
  r <- raster::resample(r, trast)
  v <- raster::getValues(r)
  v[is.na(v)] <- 0
  r <- raster::setValues(r, v)

  res <- list(ud = r, model = model)
  class(res) <- c("akde", "hr", class(res))
  res
}

