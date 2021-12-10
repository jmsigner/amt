#' Difference in x and y
#'
#' Difference in x and y coordinates.
#'
#' @param x A track_xy{t}.
#' @template dots_none
#' @return Numeric vector
#' @name diff
#' @export

#' @export
#' @rdname diff
diff_x <- function(x, ...) {
  UseMethod("diff_x", x)
}


#' @export
diff_x.track_xy <- function(x, ...) {
   c(diff_rcpp(x$x_), NA)
}

#' @export
#' @rdname diff
diff_y <- function(x, ...) {
  UseMethod("diff_y", x)
}

#' @export
diff_y.track_xy <- function(x, ...) {
   c(diff_rcpp(x$y_), NA)
}



