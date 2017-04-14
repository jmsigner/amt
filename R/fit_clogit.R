#' @export
fit_clogit <- function(data, ...) {
  UseMethod("fit_clogit", data)
}

#' @export
fit_clogit.random_steps <- function(data, formula, more = NULL, ...) {
  m <- survival::clogit(formula, data = data, ...)
  m <- list(model = m,
            sl_ = attributes(data)$sl_,
            ta_ = attributes(data)$ta_,
            more = more)
  class(m) <- c("fit_clogit", class(m))
  m
}

#' @export
coef.fit_clogit <- function(object, ...) {
  stats::coef(object$model, ...)
}


#' @export
summary.fit_clogit <- function(object, ...) {
  base::summary(object$model, ...)
}
