#' Extract covariate values.
#'
#' Functions to extract the value of covariates for tracks or steps.
#' @param x A track or step.
#' @param covariates A `RasterLayeR` or a `RasterStack`.
#' @param where A character scalar, for `steps` this can take the values `start` or `end`.
#' @template dots_none
#' @name extract_covariates
#' @export
extract_covariates <- function(x, ...) {
  UseMethod("extract_covariates", x)
}

#' @export
#' @rdname extract_covariates
extract_covariates.track_xy <- function(x, covariates, ...) {
  extract_covar_base(x, covariates)
}

#' @export
#' @rdname extract_covariates
extract_covariates.random_points <- function(x, covariates, ...) {
  extract_covar_base(x, covariates)
}

#' @export
#' @rdname extract_covariates
extract_covariates.steps <- function(x, covariates, where = "end", ...) {
  if (class(covariates) %in% paste0("Raster", c("Layer", "Stack", "Brick"))) {
    x[names(covariates)] <- if (where == "end") {
      raster::extract(covariates, x[, c("x2_", "y2_")])
    } else {
      raster::extract(covariates, x[, c("x1_", "y1_")])
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
