#' @importFrom dplyr arrange filter group_by select summarize summarise ungroup rename
#' @importFrom dplyr distinct ungroup mutate mutate_at pull %>% bind_rows select_vars n
#' @importFrom graphics plot legend lines par points
#' @importFrom grDevices adjustcolor
#' @importFrom lubridate hours minutes seconds now days weeks
#' @importFrom magrittr %>%
#' @importFrom stats coef dgamma median na.omit qgamma quantile runif sd var formula reformulate qnorm
#' @importFrom tidyr nest unnest
#' @importFrom tibble tibble tribble is_tibble as_tibble
#' @importFrom utils data head tail
#' @importFrom sf NA_crs_
#' @importFrom sp CRS
#' @importFrom stats dexp qexp terms predict
#' @importFrom methods as is
#' @importFrom purrr map map_int
#' @importFrom Rcpp evalCpp
#' @importFrom rlang quo quos enquo quo_name
#' @importFrom raster raster
#' @import survival
#' @import Rdpack
#' @export

methods::setOldClass(c("track_xy", "tbl_df", "tbl", "data.frame"))
methods::setOldClass(c("track_xyt", "track_xy", "tbl_df", "tbl", "data.frame"))
methods::setOldClass(c("random_points", "tbl_df", "tbl", "data.frame"))
methods::setOldClass(c("steps", "tbl_df", "tbl", "data.frame"))
methods::setOldClass(c("random_steps", "tbl_df"))

utils::globalVariables(
  c("burst_", "step_id_", "t_", "case_", ".data",
    "ts", "sl_", "xs", "ys", "x_", "y_", "x1_", "y1_",
    "y0_", "x0_", "y2_", "x2_", "abs.dir2", "rel.dir", "ta_",
    "step_id_1", "abs.dir1", "x", "y")) # to omit CRAN notes

#' @useDynLib amt

"_PACKAGE"


# exports from other packages ---------------------------------------------

#' @export
dplyr::arrange

#' @export
dplyr::filter

#' @export
dplyr::group_by
#' @export
dplyr::select
#' @export
dplyr::summarise
#' @export
dplyr::summarize
#' @export
dplyr::ungroup
#' @export
dplyr::distinct
#' @export
dplyr::ungroup
#' @export
dplyr::mutate
#' @export
dplyr::pull
#' @export
lubridate::hours
#' @export
lubridate::minutes
#' @export
lubridate::seconds
#' @export
lubridate::days
#' @export
lubridate::weeks
#' @export
purrr::map
#' @export
purrr::map_int
#' @export
sf::NA_crs_
#' @export
survival::clogit
#' @export
survival::strata
#' @export
tidyr::nest
#' @export
tidyr::unnest
#' @export
tibble::tibble
#' @export
survival::Surv
