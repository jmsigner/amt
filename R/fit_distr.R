#' Fit a Step-Lengths distribution.
#'
#' Fit a distribution to the the step lengths of a track.
#' @param .tbl A `track_xy*`.
#' @param x The column, containing step lengths, usually `sl_`.
#' @param distr Name of the distribution, curently only `gamma` distribution is supported.
#' @param na.rm Logical scalar, should `NA` be removed?
#' @tempalte dots_none
#' @name fit_sl_dist
#'
#' @export
#' @rdname fit_sl_dist
fit_sl_dist_ <- function(.tbl, x, ...){
  fit_sl_dist_base(.tbl[[x]], ...)
}

#' @export
#' @rdname fit_sl_dist
fit_sl_dist <- function(.tbl, x, ...){
  fit_sl_dist_base(.tbl[deparse(substitute(x))], ...)
}


#' @export
#' @rdname fit_sl_dist
fit_sl_dist_base <- function(x, na.rm = TRUE, distr = "gamma", ...) {
  if (is.list(x)) x <- unlist(x, use.names = FALSE, recursive = TRUE)
  if (na.rm) x <- x[!is.na(x)]

  if (any(x == 0)) {
    stop("0 length steps are not possible, consider adding a random error")
  }

  fitdistrplus::fitdist(x, distr)

}

#' Step lengths parameters
#'
#' Returns the parameter of the distribution (e.g., gamma) fitted to the distribution of step lengths.
#' @param x An object that contains either a fitted conditional logistic regression, random steps or just a fitted distribution.
#' @export
#' @name sl_params
sl_params <- function (x, ...) {
  UseMethod("sl_params", x)
}

#' @export
#' @rdname sl_params
sl_params.fit_clogit <- function (x, ...) {
  sl_params(x$sl_)
}

#' @export
#' @rdname sl_params
sl_params.random_steps <- function (x, ...) {
  sl_params(attributes(x)$sl_)
}

#' @export
#' @rdname sl_params
sl_params.fitdist <- function(x, ...) {
  if (x$distname == "gamma") {
    if ("scale" %in% names(x$estimate)) {
      c(shape = unname(x$estimate["shape"]),
        scale = unname(x$estimate["scale"]))
    } else {
      c(shape = unname(x$estimate["shape"]),
        scale = 1 / unname(x$estimate["rate"]))
    }
  } else {
    stop("Function only works for gamma distr at the moment.")
  }
}

#' Step-length distribution
#'
#' Returns the name of the distribution (e.g., gamma) fitted to the distribution of step lengths.
#' @param x An object that contains either a fitted conditional logistic regression, random steps or just a fitted distribution.
#' @export
#' @name sl_distr
sl_distr <- function (x, ...) {
  UseMethod("sl_distr", x)
}

#' @export
#' @rdname sl_distr
sl_distr.fitdist <- function(x, ...) {
  x$distname
}

#' @export
#' @rdname sl_distr
sl_distr.fit_clogit <- function (x, ...) {
  sl_distr(x$sl_)
}

#' @export
#' @rdname sl_distr
sl_distr.random_steps <- function (x, ...) {
  sl_distr(attributes(x)$sl_)
}


# turning angles ----------------------------------------------------------
#' Fit a turning-angles distribution.
#'
#' Fit a distribution to the the turning angles of a track.
#' @param .tbl A `track_xy*`.
#' @param x The column, containing the relative turning angles, usually `ta_`.
#' @param distr Name of the distribution, curently only `vonmises` distribution is supported.
#' @param na.rm Logical scalar, should `NA` be removed?
#' @tempalte dots_none
#' @name fit_ta_dist

#' @export
#' @rdname fit_ta_dist
fit_ta_dist_ <- function(.tbl, x, ...){
  fit_ta_dist_base(.tbl[[x]], ...)
}

#' @export
#' @rdname fit_ta_dist
fit_ta_dist <- function(.tbl, x, ...){
  fit_ta_dist_base(.tbl[deparse(substitute(x))], ...)
}


#' @export
#' @rdname fit_ta_dist
fit_ta_dist_base <- function(x, na.rm = TRUE, distr = "vonmises", ...) {
  if (is.list(x)) x <- unlist(x, use.names = FALSE, recursive = TRUE)
  if (na.rm) x <- x[!is.na(x)]

  x <- circular::as.circular(x, type = "angles", units = "degrees", template = "none",
                             modulo = "asis", zero = 0, rotation = "counter")
  circular::mle.vonmises(x, ...)
}


