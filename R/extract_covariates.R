#' @export
extract_covariates <- function(x, ...) {
  UseMethod("extract_covariates", x)
}

#' @export
extract_covariates.track_xy <- function(x, covariates, ...) {
  extract_covar_base(x, covariates)
}

#' @export
extract_covariates.random_points <- function(x, covariates, ...) {
  extract_covar_base(x, covariates)
}

#' @export
extract_covariates.steps <- function(x, covars, where = "end", ...) {
  if (class(covars) %in% paste0("Raster", c("Layer", "Stack", "Brick"))) {
    x[names(covars)] <- if (where == "end") {
      raster::extract(covars, x[, c("x2_", "y2_")])
    } else {
      raster::extract(covars, x[, c("x1_", "y1_")])
    }
    x
  } else {
    stop("no raster")
  }
}

extract_covar_base <- function(x, covars) {
  if (class(covars) %in% paste0("Raster", c("Layer", "Stack", "Brick"))) {
    x[names(covars)] <- raster::extract(covars, x[, c("x_", "y_")])
    x
  } else {
    stop("no raster")
  }
}
