#' @rdname hrest
#' @export

hr_akde <- function(x, ...) {
  UseMethod("hr_akde", x)
}


#' @export
#' @rdname hrest
hr_akde.track_xyt <- function(x, model = fit_ctmm(x, "iid"), keep.data = TRUE,
                              trast = make_trast(x), levels = 0.95, wrap = FALSE,
                              ...) {

  if (grepl("bm", tolower(summary(model)$name))) {
    warning("Brownian motion was chosen as movement model, akde will not work")
  }

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

  res <- list(ud = if(wrap) terra::wrap(point.est) else point.est,
              akde = ud,
              model = model, levels = levels,
              trast = if (wrap) terra::wrap(trast) else trast,
              estimator = "akde",
              crs = get_crs(x),
              data = if (keep.data) x else NULL)
  class(res) <- c("akde", "hr_prob", "hr", class(res))
  res
}

