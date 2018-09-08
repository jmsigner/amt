#' Extract covariate values
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
extract_covariates.steps_xy <- function(x, covariates, where = "end", ...) {
  if (class(covariates) %in% paste0("Raster", c("Layer", "Stack", "Brick"))) {
    if (where == "both") {
      x_start <- raster::extract(covariates, as.matrix(x[, c("x1_", "y1_")]),
                                 df = TRUE)[, -1, drop = FALSE]
      names(x_start) <- paste0(names(x_start), "_start")
      x_end <- raster::extract(covariates, as.matrix(x[, c("x2_", "y2_")]),
                               df = TRUE)[, -1, drop = FALSE]
      names(x_end) <- paste0(names(x_end), "_end")
      x_all <- cbind(x_start, x_end)
      x[names(x_all)] <- x_all
    } else {
      x[names(covariates)] <- if (where == "end") {
        raster::extract(covariates, as.matrix(x[, c("x2_", "y2_")]))
      } else if (where == "start") {
        raster::extract(covariates, as.matrix(x[, c("x1_", "y1_")]))
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


# extract covariates along ------------------------------------------------


#' @rdname extract_covariates
#' @details `extract_covariates_along` extracts the covariates along a straight line between the start and the end point of a (random) step. It returns a list, which in most cases will have to be processed further.
#' @export
#' @examples
#' data(deer) # relocation
#' data("sh_forest") # env covar
#'
#' p1 <- deer %>% steps() %>% random_steps() %>%
#'   extract_covariates(sh_forest) %>% # extract at the endpoint
#'   mutate(for_path = extract_covariates_along(., sh_forest))  %>%
#'   # 1 = forest, lets calc the fraction of forest along the path
#'   mutate(for_per = purrr::map_dbl(for_path, ~ mean(. == 1)))
#'
extract_covariates_along <- function(x, ...) {
  UseMethod("extract_covariates_along", x)
}

#' @export
#' @rdname extract_covariates
extract_covariates_along.steps_xy <- function(x, covariates, ...) {
  if (class(covariates) %in% paste0("Raster", c("Layer", "Stack", "Brick"))) {
    wkt <- with(x, paste0("LINESTRING (", x1_, " ", y1_, ",", x2_, " ", y2_, ")"))
    ll <- sf::st_as_sfc(wkt)
    v <- velox::velox(covariates)
    l2 <- v$extract(sp = ll)
    return(l2)
  } else {
    stop("covariates: need to be a Raster*.")
  }
}
