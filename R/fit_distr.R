#' @export
fit_sl_dist_ <- function(.tbl, x, ...){
  fit_sl_dist_base(.tbl[[x]], ...)
}

#' @export
fit_sl_dist <- function(.tbl, x, ...){
  fit_sl_dist_base(.tbl[deparse(substitute(x))], ...)
}


#' @export
fit_sl_dist_base <- function(x, na.rm = TRUE, distr = "gamma", ...) {
  if (is.list(x)) x <- unlist(x, use.names = FALSE, recursive = TRUE)
  if (na.rm) x <- x[!is.na(x)]

  if (any(x == 0)) {
    stop("0 length steps are not possible, consider adding a random error")
  }

  fitdistrplus::fitdist(x, distr)

}

#' @export
sl_params <- function (x, ...) {
  UseMethod("sl_params", x)
}

#' @export
sl_params.fit_clogit <- function (x, ...) {
  sl_params(x$sl_)
}

#' @export
sl_params.random_steps <- function (x, ...) {
  sl_params(attributes(x)$sl_)
}

#' @export
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

#' @export
sl_distr <- function (x, ...) {
  UseMethod("sl_distr", x)
}

#' @export
sl_distr.fitdist <- function(x, ...) {
  x$distname
}

#' @export
sl_distr.fit_clogit <- function (x, ...) {
  sl_distr(x$sl_)
}

#' @export
sl_distr.random_steps <- function (x, ...) {
  sl_distr(attributes(x)$sl_)
}


# turning angles ----------------------------------------------------------


#' @export
fit_ta_dist_ <- function(.tbl, x, ...){
  fit_ta_dist_base(.tbl[[x]], ...)
}

#' @export
fit_ta_dist <- function(.tbl, x, ...){
  fit_ta_dist_base(.tbl[deparse(substitute(x))], ...)
}


#' @export
fit_ta_dist_base <- function(x, na.rm = TRUE, distr = "vonmises", ...) {
  if (is.list(x)) x <- unlist(x, use.names = FALSE, recursive = TRUE)
  if (na.rm) x <- x[!is.na(x)]

  x <- circular::as.circular(x, type = "angles", units = "radians", template = "none",
                             modulo = "asis", zero = 0, rotation = "counter")
  circular::mle.vonmises(x, ...)
}


