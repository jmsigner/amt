# #' Check if tibble is a track
# #'
# #' Returns \code{TRUE} if a tibble can safely be considered as a \code{track*}.
# #'
# #' @param x A tibble.
# #' @return \code{TRUE} if tracks is as expected, otherwise \code{FALSE}.
#
#
# #' @export
# #' @rdname is_track
# is_track_xy <- function(x) {
#   is(x, "track_xy")
# }
#
# #' @export
# #' @rdname is_track
# is_track_xyz <- function(x) {
#   is(x, "track_xyz")
# }


#' @export

is_burst <- function(x) {
  if (check_trackXY(x)) {
    if ("burst_" %in% names(x)) {
      TRUE
    } else {
      FALSE
    }
  }
}

#' @export
centroid <- function(x, ...) {
  UseMethod("centroid", x)
}

#' @export
centroid.track_xy <- function(x, ...) {
  colMeans(trk[, c("x_", "y_")])
}

####################### most likely ot go

# #' Check if tibble is a track
# #'
# #' Returns \code{TRUE} if a tibble can safely be considered as a \code{track*} and error otherwise.
# #'
# #' @param x A tibble.
# #' @return \code{TRUE} if tracks is as expected, otherwise an error is thrown.
# #' @rdname check_track
# #' @export
#
# check_trackXY <- function(x) {
#   if (is_trackXY(x)) {
#     return(TRUE)
#   } else {
#     stop("x is not an object of class trackXY")
#   }
# }
#
# #' @rdname check_track
# #' @export
#
# check_trackXYZ <- function(x) {
#   if (is_trackXYZ(x)) {
#     return(TRUE)
#   } else {
#     stop("x is not an object of class trackXYZ")
#   }
# }
#
# #' @rdname check_track
# #' @export
#
# check_trackXYT <- function(x) {
#   if (is_trackXYT(x)) {
#     return(TRUE)
#   } else {
#     stop("x is not an object of class trackXYT")
#   }
# }
#
# #' @rdname check_track
# #' @export
#
# check_trackXYZT <- function(x) {
#   if (is_trackXYZT(x)) {
#     return(TRUE)
#   } else {
#     stop("x is not an object of class trackXYZT")
#   }
# }
#
# #' @rdname check_track
# #' @export
# check_burst <- function(x) {
#   if (is_burst(x)) {
#     return(TRUE)
#   } else {
#     stop("x is no burst")
#   }
# }
#
#
# #' @export
# head_tail <- function(x, n = 6) {
#   rbind(
#     head(x, n = n),
#     tail(x, n = n)
#   )
#
# }

