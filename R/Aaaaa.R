#' @importFrom dplyr arrange filter group_by select summarize summarise
#' @importFrom dplyr distinct ungroup mutate mutate_at pull %>% bind_rows select_vars
#' @importFrom graphics plot
#' @importFrom grDevices adjustcolor legend
#' @importFrom lazyeval lazy_dots lazy
#' @importFrom lubridate hours minutes seconds now
#' @importFrom rlang enquo quos
#' @importFrom methods is
#' @importFrom purrr map
#' @importFrom sp CRS SpatialPoints
#' @importFrom stats coef dgamma median na.omit qgamma quantile runif sd
#' @importFrom tidyr nest unnest
#' @importFrom tibble as_data_frame data_frame tibble tribble is_tibble as_tibble
#' @importFrom utils data head tail
#' @import survival tidyverse

methods::setOldClass(c("track_xy", "tbl_df", "tbl", "data.frame"))
methods::setOldClass(c("track_xyt", "track_xy", "tbl_df", "tbl", "data.frame"))
methods::setOldClass(c("random_points", "tbl_df", "tbl", "data.frame"))
methods::setOldClass(c("steps", "tbl_df", "tbl", "data.frame"))
methods::setOldClass(c("random_steps", "tbl_df"))
NULL

#' @useDynLib amt
NULL
