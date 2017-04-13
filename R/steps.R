#' @export
steps_by_burst <- function(x, ...) {
  UseMethod("steps_by_burst", x)
}

#' @export
steps_by_burst.track_xyt <- function(x, ...) {

  nest_cols <- select_vars_(colnames(x), "-burst_")
  xx <- nest_(x, key_col = "data", nest_cols)

  xx$data <- map(xx$data, steps)
  xx <- unnest(xx)
  class(xx) <- c("steps", class(x)[-(1:2)])
  xx
}


#' @export
steps <- function(x, ...) {
  UseMethod("steps", x)
}

#' @export
steps.track_xy <- function(x, ...) {
  n <- nrow(x)
  xx <- steps_base(x, n)
  class(xx) <- c("steps", class(x)[-1])
  xx
}

#' @export
steps.track_xyt <- function(x, ...) {
  n <- nrow(x)
  if ("burst_" %in% names(x)) {
    warning("burst's are ignored, use steps_by_burst instead.")
  }
  xx <- steps_base(x, n)
  xx$t1_ <- x$t_[-n]
  xx$t2_ <- x$t_[-1]
  xx$dt_ <- xx$t2_ - xx$t1_
  class(xx) <- c("steps", class(x)[-(1:2)])
  xx
}


steps_base <- function(x, n) {
  data_frame(
    x1_ = x$x_[-n],
    x2_ = x$x_[-1],
    y1_ = x$y_[-n],
    y2_ = x$y_[-1],
    sl_ = step_lengths(x)[-n],
    ta_ = direction_rel(x)[-n]
  )
}

steps_transfer_attr <- function(from, to) {
  from <- attributes(from)
  attributes(to)$class <- from$class
  attributes(to)$sl_ <- from$sl_
  attributes(to)$ta_ <- from$ta_
  to
}

methods::setOldClass(c("steps", "tbl_df"))
#' @export
`[.steps` <- function(x, i, j, drop = FALSE) {
  xx <- NextMethod()
  steps_transfer_attr(x, xx)
}

#' @export
`[[.steps` <- function(x, i, j, ..., exact = TRUE) {
  xx <- NextMethod()
  steps_transfer_attr(x, xx)
}

# see here: https://github.com/hadley/dplyr/issues/719
#' @export
arrange_.steps <- function(.data, ..., .dots) {
  xx <- NextMethod()
  steps_transfer_attr(.data, xx)
}

#' @export
filter_.steps <- function(.data, ..., .dots) {
  xx <- NextMethod()
  steps_transfer_attr(.data, xx)
}

#' @export
group_by_.steps <- function(.data, ..., .dots) {
  xx <- NextMethod()
  steps_transfer_attr(.data, xx)
}

#' @export
mutate_.steps <- function(.data, ..., .dots) {
  xx <- NextMethod()
  steps_transfer_attr(.data, xx)
}

#' @export
select_.steps <- function(.data, ..., .dots) {
  xx <- NextMethod()
  steps_transfer_attr(.data, xx)
}

#' @export
summarise_.steps <- function(.data, ..., .dots) {
  xx <- NextMethod()
  steps_transfer_attr(.data, xx)
}


#' @export
summarize_.steps <- function(.data, ..., .dots) {
  xx <- NextMethod()
  steps_transfer_attr(.data, xx)
}

