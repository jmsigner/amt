#' Home ranges
#'
#' Functions to calculate animal home ranges from a `track_xy*`, and to work
#' with home ranges. `hr_mcp` and `hr_locoh` calculate the minimum convex
#' polygon and local convex hull home range respectively. `hr_area` extracts the
#' area of an home range, `hr_isopleths` returns the isopleth as a
#' `SpatialPolygonsDataFrame`.
#'
#' @template track_xy_star
#' @param levels `[numeric]` \cr The isopleth levels used for calculating home
#'   ranges. Should be `0 < level < 1`.
#' @param n `[integer(1)]` \cr The number of neighbors used when calculating
#'   local convex hulls.
#' @param rand_buffer `[numeric(1)]` \cr Random buffer to avoid polygons with
#'   area 0 (if coordinates are numerically identical).
#' @template dots_none
#' @name hr
#' @examples
#' data(deer)
#'
#'
#' # MCP ---------------------------------------------------------------------
#' mcp1 <- hr_mcp(deer)
#' hr_area(mcp1)
#'
#' # calculated MCP at different levels
#' mcp1 <- hr_mcp(deer, levels = seq(0.3, 1, 0.1))
#' hr_area(mcp1)
#'
#' # CRS are inherited
#' get_crs(deer)
#' mcps <- hr_mcp(deer, levels = c(0.5, 0.95, 1))
#' has_crs(mcps)
#'
#' # Local Convex Hull (LoCoH) -----------------------------------------------
#' locoh1 <- hr_locoh_k(deer)
#' hr_area(locoh1)
#'
#' # calculated MCP at different levels
#' locoh <- hr_locoh_k(deer, levels = seq(0.3, 1, 0.1))
#' hr_area(locoh)
#'
#' # CRS are inherited
#' get_crs(deer)
#' get_crs(locoh1)
#'
NULL




