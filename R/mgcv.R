# These functions provide support for mgcv models

#' Check for `mgcv`
#'
#' Internal function to stop if `mgcv` is not installed
check_mgcv <- function() {
  if (!rlang::is_installed("mgcv")) {
    stop("You must install package 'mgcv' for this function to work.")
  }
}

#' Format data for `mgcv`
#'
#' Adds columns to data for use in (i)SSF fitting via `mgcv::gam()`
#'
#' @param object `[data.frame]` \cr The formatted data. Often an object of
#' class `random_steps` (which is ultimately a `data.frame`).
#' @template dots_none
#'
#' @details This function currently only adds a single column of 1s to the
#' data, labeled `times_` following Klappstein et al. (2024).
#'
#' @author Brian J. Smith
#'
#' @return Returns data with same class as input.
#'
#' @references
#' Klappstein, N.J., Michelot, T., Fieberg, J., Pedersen, E.J. & Mills-Flemming, J. (2024). Step selection functions with non‐linear and random effects. *Methods in Ecology and Evolution*, 15(8), 1332-1346. doi: 10.1111/2041-210X.14367
#'
#' @examples
#'
#' \donttest{
#'
#' # Load packages
#' library(mgcv)
#'
#' # Load data
#' data("deer")
#'
#' # Prepare data for SSF
#' ssf_data <- deer |>
#'   steps_by_burst() |>
#'   random_steps(n = 15) |>
#'   extract_covariates(sh_forest) |>
#'   mutate(forest = factor(forest, levels = 1:0,
#'                          labels = c("forest", "non-forest"))) |>
#'   # Prepare data for mgcv
#'   prep_mgcv()
#'
#' }
#' @export
prep_mgcv <- function(object){
  if (!inherits(object, "data.frame")) {
    stop("Argument `object` must be a data.frame (or tibble).")
  }

  object$times_ <- 1
  return(object)
}
