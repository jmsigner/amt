# #' Detect immobile periods
# #'
# #' Function to find periods where an animal is immobile (within a spatial range) for a given amount of time, i.e. it does not move.
# #'
# #' @param x A \code{trackXYT}.
# #' @param period Time threshold that defines immobility, object of class \code{lubridate::Period}.
# #' @param spatial_tolerance Distance threshold for consecutive relocations that define immobility in units of the coordinate reference system.
# #'
# #' @return A vector of length \code{nrow(x)}, with 1 for fixes that are before an immobility period, and -1 for fixes that are within and after the immobility period.
# #' @export

track_immobile_base <- function(x, period = hours(2), spatial_tolerance = 0) {
    track_immobility(x$t_, x$x_, x$y_, lubridate::period_to_seconds(period), spatial_tolerance)
}

track_immobile_base <- function(x, period, spatial_tolerance) {
}
