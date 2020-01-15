#' @rdname hr
#' @export
hr_akde <- function(x, ...) {
  UseMethod("hr_akde", x)
}


#' @export
#' @param model A continous time movement model. This can be fitted either with `ctmm::ctmm.fit` or `fit_ctmm`.
#' @rdname hr
hr_akde.track_xyt <- function(x, model = fit_ctmm(x, "bm"),
                              trast = make_trast(x), ...) {

  x <- deer

  outpts <- sp::spTransform(raster::rasterToPoints(trast, spatial = TRUE),
                            CRS("+init=epsg:4326"))

  trast <- make_trast(x)
  suppressMessages(suppressWarnings(dat <- as_telemetry(x)))
  ud <- ctmm::akde(dat, CTMM = model)#, grid = list(r = outpts))


  r <- 1 - ctmm::raster(krige, DF = "CDF")
  r <- raster::projectRaster(r, to = trast)
  r <- raster::resample(r, trast)
  v <- raster::getValues(r)
  v[is.na(v)] <- 0
  r <- raster::setValues(r, v)
  attr(r, "model") <- model

}

