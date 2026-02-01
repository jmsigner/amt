# library(amt)
# library(sf)
# library(ggplot2)
#
# # Funciton that could eventually go to `amt`
#
# distance_to_feature <- function(x, ...) {
#   UseMethod("distance_to_feature", x)
# }
#
# #' @export
# #' @param y An sf object
# distance_to_feature.track_xy <- function(x, y, ...) {
#   x[[deparse(substitute(y))]] <- distance_to_feature_base(as_sf(x), y)
#   x
# }
#
# #' @export
# #' @param end `[logical(1)=TRUE]` \cr For steps, should the end or start points be used?
# distance_to_feature.steps_xy <- function(x, y, where = "end", ...) {
#   checkmate::assert_choice(where, c("end", "start", "both"))
#
#   if (where %in% c("end", "both"))
#     x[[paste0(deparse(substitute(y)), "_end")]] <- distance_to_feature_base(as_sf(x, end = end), y)
#   if (where %in% c("start", "both"))
#   x[[paste0(deparse(substitute(y)), "_start")]] <- distance_to_feature_base(as_sf(x, end = end), y)
#
#   x
# }
#
#
# distance_to_feature_base <- function(x, y) {
#   y <- sf::st_union(y)
#   st_distance(x, y)[, 1] |> units::drop_units()
# }
#
#
# x <- st_as_sf(data.frame(x = 1:10, y = 1), coords = c("x", "y"))
# y <- st_as_sf(data.frame(x = 5, y = 4:7), coords = c("x", "y"))
#
# st_distance(x, y) |> apply(1, min)
# st_distance(x, st_union(y))[, 1]
#
#
# # Example
# library(amt)
# data("deer")
#
# identical(
#   deer |> get_crs(),
#   steps_by_burst(deer) |> get_crs())
#
# # Create a line around all locations and the distance to this line
# ls <- st_convex_hull(as_sf(deer) |> st_union()) |> st_cast("LINESTRING")
#
# cc <- ls |> st_coordinates()
#
# plot(l1 <- st_sfc(list(st_linestring(cc[c(2, 8), 1:2])), crs = st_crs(ls)))
#
# distance_to_feature(deer, l1)
# distance_to_feature(deer |> steps(), l1)
#
#
# d1 <- distance_to_feature(deer, ls)
# d2 <- distance_to_feature(deer |>  steps(), ls)
# d3 <- distance_to_feature(deer |>  steps() |> random_steps(), ls)
#
# # Verify
# d1 |>
#   ggplot(aes(x_, y_, col = ls)) + geom_point()
