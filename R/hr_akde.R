#' @rdname hrest
#' @export

hr_akde <- function(x, ...) {
  UseMethod("hr_akde", x)
}


#' @export
#' @rdname hrest
hr_akde.track_xyt <- function(x, model = fit_ctmm(x, "iid"), keep.data = TRUE,
                              trast = make_trast(x), levels = 0.95, ...) {

  if (grepl("bm", tolower(summary(model)$name))) {
    warning("Brownian motion was chosen as movement model, akde will not work")
  }

  ##
  x <- deer
  model = fit_ctmm(x, "iid")
  keep.data = TRUE
  trast = make_trast(x)
  levels = 0.95
  ##

  suppressMessages(suppressWarnings(dat <- as_telemetry(x)))
  ud <- ctmm::akde(dat, model)

  ctmm2rast <- function(x, trast) {
    r <- terra::rast(ctmm::raster(x, DF = "PDF"))
    r <- terra::project(r, trast)
    r <- terra::resample(r, trast)
    v <- terra::values(r)
    v[is.na(v[, 1]), 1] <- 0
    terra::values(r) <-  v
    r
  }

  point.est <- ctmm2rast(ud, trast)

  res <- list(ud = point.est, akde = ud,
              model = model, levels = levels, trast = trast, estimator = "adke",
              crs = get_crs(x),
              data = if (keep.data) x else NULL)
  class(res) <- c("akde", "hr_prob", "hr", class(res))
  res
}

