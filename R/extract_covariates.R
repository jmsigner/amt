#' Extract covariate values.
#'
#' Extract the covariate values at relocations, or at the beginning or end of
#' steps.
#' @template track_xy_star_steps
#' @param covariates `[RasterLayer,RasterStack,RasterBrick]` \cr The
#'   (environmental) covariates.
#' @param where `[character(1)="end"]{"start", "end", "both"}` \cr For `steps` this
#'   determines if the covariate values should be extracted at the beginning or
#'   the end of a step. or `end`.
#' @template dots_none
#' @name extract_covariates
#' @export
#' @examples
#' data(deer)
#' data(sh_forest)
#' deer %>% extract_covariates(sh_forest)
#' deer %>% steps %>% extract_covariates(sh_forest)
#' deer %>% steps %>% extract_covariates(sh_forest, where = "start")

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
    if (where == "both") {
      x_start <- raster::extract(covariates, x[, c("x1_", "y1_")], df = TRUE)[, -1]
      names(x_start) <- paste0(names(x_start), "_start")
      x_end <- raster::extract(covariates, x[, c("x2_", "y2_")], df = TRUE)[, -1]
      names(x_end) <- paste0(names(x_end), "_end")
      x_all <- cbind(x_start, x_end)
      x[names(x_all)] <- x_all
    } else {
      x[names(covariates)] <- if (where == "end") {
        raster::extract(covariates, x[, c("x2_", "y2_")])
      } else if (where == "start") {
        raster::extract(covariates, x[, c("x1_", "y1_")])
      }
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
