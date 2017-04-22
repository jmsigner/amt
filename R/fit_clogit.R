#' Fit a conditional logistic regression
#'
#' Tihs function is a wrapper around `stats::glm`, making it usable in a piped workflow.
#' @param data The data.
#' @param formula The formula
#' @param more Optional list returned in the output.
#' @param ... Addtional arguments, passed to `survival::clogit`.
#' @export

fit_clogit <- function(data, formula, more = NULL, ...) {
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
