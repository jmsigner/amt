#' @importFrom dplyr arrange arrange_ filter filter_ group_by group_by_ select select_ summarize summarize_ summarise summarise_
#' @importFrom dplyr distinct distinct_ ungroup
#' @importFrom dplyr mutate %>%
#' @importFrom dplyr bind_rows
#' @importFrom dplyr  select_vars
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @importFrom lazyeval lazy_dots lazy
#' @importFrom tibble as_data_frame data_frame tibble tribble
#' @importFrom tidyr nest unnest
#' @importFrom grDevices adjustcolor
#' @importFrom graphics legend
#' @importFrom methods is
#' @importFrom stats coef dgamma median na.omit qgamma quantile runif sd
#' @importFrom graphics plot
#' @importFrom utils data head tail
#' @importFrom lubridate hours minutes seconds now
#' @importFrom sp CRS SpatialPoints
#' @import survival tidyverse

methods::setOldClass(c("track_xy", "tbl_df", "tbl", "data.frame"))
methods::setOldClass(c("track_xyt", "track_xy", "tbl_df", "tbl", "data.frame"))
methods::setOldClass(c("random_points", "tbl_df", "tbl", "data.frame"))
methods::setOldClass(c("steps", "tbl_df", "tbl", "data.frame"))
methods::setOldClass(c("random_steps", "tbl_df"))
NULL

#' @useDynLib amt
NULL
