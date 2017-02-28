#' Check if tibble is a track
#'
#' Returns \code{TRUE} if a tibble can safely be considered as a \code{track*}.
#'
#' @param x A tibble.
#' @return \code{TRUE} if tracks is as expected, otherwise \code{FALSE}.


#' @export
#' @rdname is_track

is_trackXY <- function(x) {
  if (all(c("x_", "y_") %in% names(x))) {
    TRUE
  } else {
    FALSE
  }
}

#' @export
#' @rdname is_track

is_trackXYZ <- function(x) {
  if (all(c("x_", "y_", "z_") %in% names(x))) {
    TRUE
  } else {
    FALSE
  }
}

#' @export
#' @rdname is_track

is_trackXYT <- function(x) {
  if (all(c("x_", "y_", "t_") %in% names(x)) && lubridate::is.timepoint(x$t_)) {
    TRUE
  } else {
    FALSE
  }
}

#' @export
#' @rdname is_track

is_trackXYZT <- function(x) {
  if (all(c("x_", "y_", "z_", "t_") %in% names(x)) && lubridate::is.timepoint(x$t_)) {
    TRUE
  } else {
    FALSE
  }
}

#' Check if tibble is a track
#'
#' Returns \code{TRUE} if a tibble can safely be considered as a \code{track*} and error otherwise.
#'
#' @param x A tibble.
#' @return \code{TRUE} if tracks is as expected, otherwise an error is thrown.
#' @rdname check_track
#' @export

check_trackXY <- function(x) {
  if (is_trackXY(x)) {
    return(TRUE)
  } else {
    stop("x is not an object of class trackXY")
  }
}

#' @rdname check_track
#' @export

check_trackXYZ <- function(x) {
  if (is_trackXYZ(x)) {
    return(TRUE)
  } else {
    stop("x is not an object of class trackXYZ")
  }
}

#' @rdname check_track
#' @export

check_trackXYT <- function(x) {
  if (is_trackXYT(x)) {
    return(TRUE)
  } else {
    stop("x is not an object of class trackXYT")
  }
}

#' @rdname check_track
#' @export

check_trackXYZT <- function(x) {
  if (is_trackXYZT(x)) {
    return(TRUE)
  } else {
    stop("x is not an object of class trackXYZT")
  }
}


#' @export
head_tail <- function(x, n = 6) {
  rbind(
    head(x, n = n),
    tail(x, n = n)
  )

}
