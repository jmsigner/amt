#' Home ranges
#'
#' Functions to calculate animal home ranges from a `track_xy*`, and to work
#' with home ranges. `hr_mcp`, `hr_kde`, and `hr_locoh` calculate the minimum convex
#' polygon, kernel density, and local convex hull home range respectively. `hr_area` extracts the
#' area of an home range, `hr_isopleths` returns the isopleth as a
#' `SpatialPolygonsDataFrame`.
#'
#' The implementation of the reference bandwidth calculation is based on Worton (1989). If variances differ greatly, it is advaisable to rescale the data using `rescale = "unitvar"` the data is suspected to multimodal other bandwidth estimation methods may be more suitable.
#'
#' @template track_xy_star
#' @param levels `[numeric]` \cr The isopleth levels used for calculating home
#'   ranges. Should be `0 < level < 1`.
#' @param n `[integer(1)]` \cr The number of neighbors used when calculating
#'   local convex hulls.
#' @param rand_buffer `[numeric(1)]` \cr Random buffer to avoid polygons with
#'   area 0 (if coordinates are numerically identical).
#' @param trast `[RasterLayer]` \cr A template raster for kernel density home-ranges.
#' @param h `[numeric(2)]` \cr The bandwidth for kernel density estimation.
#' @param rescale `[character(1)]` \cr Rescaling method for reference bandwidth calucation. Must be one of "unitvar", "xvar", or "none".
#' @template dots_none
#' @references Worton, B. J. (1989). Kernel methods for estimating the utilization distribution in home-range studies. _Ecology, 70(1)_, 164-168.
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
#' # Kernel density estimaiton (KDE) -----------------------------------------
#' kde1 <- hr_kde(deer)
#' plot(kde1)
#' plot(hr_isopleths(kde1, 0.95))
#' hr_area(kde1)
#' get_crs(kde1)

NULL






