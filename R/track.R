#' Create a `track_*`
#'
#' Constructor to crate a track, the basic building block of the `amt` package. A
#' `track` is usually created from a set of `x` and `y` coordinates, possibly
#' time stamps, and any number of optional columns, such as id, sex, age, etc.
#'
#' @param tbl [data.frame] \cr The `data.frame` from which a track should be
#'   created.
#' @param .x,.y,.t `[expression(1)]` \cr Unquoted variable names of columns
#'   containing the x and y coordinates, and optionally a time stamp.
#' @param ... `[expression]` \cr Additional columns from `tbl` to be used in a
#'   track. Columns should be provided in the form of `key = val` (e.g., for ids
#'   this may look like this `id = c(1, 1, 1, 2, 2, 2` for three points for ids
#'   1 and 2 each).
#' @param crs `[sp::CRS]` \cr An optional coordinate reference system of the
#'   points.
#' @param order_by_ts `[logical(1)]` \cr Should relocations be ordered by time
#'   stamp, default is `TRUE`.
#' @param check_duplicates `[logical(1)=FALSE]` \cr Should it be checked if there are
#'   duplicated time stamp, default is `FALSE`.
#' @param all_cols `[logical(1)=FALSE]` \cr Should all columns be carried over to the track object, default is `FALSE`.
#' @param x,y `[numeric]` \cr The x and y coordinates.
#' @param t `[POSIXct]` \cr The time stamp.
#' @return If `t` was provided an object of class `track_xyt` is returned
#'   otherwise a `track_xy`.
#' @export
#' @name track

mk_track <- function(tbl, .x, .y, .t, ..., crs = NULL, order_by_ts = TRUE,
                     check_duplicates = FALSE, all_cols = FALSE) {

  if (missing(.x) | missing(.y)) {
    stop("x and y are required")
  }

  if (!is_tibble(tbl)) {
    tbl <- as_tibble(tbl)
  }

  .x <- enquo(.x)
  .y <- enquo(.y)

  vars <- if (all_cols) {
    xx <- setdiff(
      names(tbl),
      c(quo_name(.x), quo_name(.y), if (!missing(.t)) quo_name(enquo(.t))))
  } else {
    quos(...)
  }

  if (!all(sub("~", "", as.character(vars)) %in% names(tbl))) {
    stop("Non existent columns from tbl were requested.")
  }


  if (missing(.t)) {
    message(".t missing, creating `track_xy`.")
    out <- tbl %>%
      dplyr::select(x_ = !!.x,
             y_ = !!.y,
             !!!vars)

    class(out) <- c("track_xy", class(out))

  } else {
    message(".t found, creating `track_xyt`.")

    .t <- enquo(.t)
    if (order_by_ts) {
      tbl <- arrange(tbl, !!(.t))
    }
    tt <- dplyr::select(tbl, t = !!.t) %>% pull(t)

    if (check_duplicates & any(duplicated(tt))) {
      stop("duplicated time stamps.")
    }

    if (any(is.na(tt))) {
      stop("NA in time stamps.")
    }

    if (any(diff(tt) < 0)) {
      stop("negative time diffs.")
    }

    out <- tbl %>%
      dplyr::select(x_ = !!.x,
                    y_ = !!.y,
                    t_ = !!.t,
                    !!!vars)
    class(out) <- c("track_xyt", "track_xy", class(out))
  }


  if (!is.null(crs)) {
    if (!is(crs, "CRS")) {
      stop("crs is no instance of class CRS")
    }
  }
  attributes(out)$crs_ <- if(is.null(crs)) NA_character_ else crs

  out
}

#' @export
#' @rdname track
make_track <- mk_track

#' @rdname track
#' @export
track <- function(x, y, t, ..., crs = NULL) {

  # .Deprecated("mk_track", msg = "Use mk_track instead")


  if (missing(x) | missing(y)) {
    stop("x and y are required")
  }

  if (missing(t)) {
    out <- tibble(
      x_ = x,
      y_ = y,
      ...
    )
   class(out) <- c("track_xy", class(out))

  } else {
    out <- tibble(
      x_ = x,
      y_ = y,
      t_ = t,
      ...
    )
   class(out) <- c("track_xyt", "track_xy", class(out))
  }

  attributes(out)$crs_ <- crs
  out
}



# S3 Methods --------------------------------------------------------------

# track_transfer_attr <- function(from, to) {
#   from <- attributes(from)
#   attributes(to)$class <- from$class
#   attributes(to)$crs_ <- from$crs_
#   to
# }

 track_transfer_attr <- function(from, to) {
   from <- attributes(from)
   attributes(to)$class <- c(setdiff(from$class, class(to)), class(to))
   attributes(to)$crs_ <- from$crs_
   to
 }

# tibble methods
#' @export
`[.track_xy` <- function(x, i, j, drop = FALSE) {
  xx <- NextMethod()
  track_transfer_attr(x, xx)
}


#' @export
`[.track_xyt` <- function(x, i, j, drop = FALSE) {
  xx <- NextMethod()
  track_transfer_attr(x, xx)
}

# dplyr methods -----------------------------------------------------------

#' @export
arrange.track_xy <- function(.data, ..., .dots) {
  x <- NextMethod()
  track_transfer_attr(.data, x)
}

#' @export
arrange.track_xyt <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
filter.track_xy <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
filter.track_xyt <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
group_by.track_xy <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
group_by.track_xyt <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
nest.track_xy <- function(.data, ..., .dots) {
 x <-  NextMethod()
 class(x) <- c("tbl_df", "tbl", "data.frame")
 x
}

#' @export
ungroup.track_xy <- function(x, ..., .dots) {
  x <- track_transfer_attr(x, NextMethod())
  class(x) <- class(x)[class(x) != "grouped_df"]
  x
}

#' @export
ungroup.track_xyt <- function(x, ..., .dots) {
  x <- track_transfer_attr(x, NextMethod())
  class(x) <- class(x)[class(x) != "grouped_df"]
  x
}

#' @export
mutate.track_xy <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
mutate.track_xyt <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}


#' @export
select.track_xy <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
select.track_xyt <- function(.data, ..., .dots) {
  track_transfer_attr(.data, NextMethod())
}

#' @export
summarise.track_xy <- function(.data, ..., .dots) {
  NextMethod()
#  track_transfer_attr(.data, NextMethod())
}

#' @export
summarise.track_xyt <- function(.data, ..., .dots) {
  NextMethod()
  # track_transfer_attr(.data, NextMethod())
}

#' @export
summarize.track_xy <- function(.data, ..., .dots) {
  NextMethod()
  # track_transfer_attr(.data, NextMethod())
}

#' @export
summarize.track_xyt <- function(.data, ..., .dots) {
  NextMethod()
  # track_transfer_attr(.data, NextMethod())
}

