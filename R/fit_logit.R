#' Fit logistic regression
#'
#' Tihs function is a wrapper around `stats::glm`, making it usable in a piped workflow.
#' @param data The data.
#' @param formula The formula
#' @param ... Further arguments passed to `stats::glm`.
#' @export
fit_logit <- function(data, formula, ...) {
  m <- stats::glm(formula, data = data, family = stats::binomial(link = "logit"), ...)
  m <- list(model = m)
  class(m) <- c("fit_logit", class(m))
  m
}

#' @export
coef.fit_logit <- function(object, ...) {
  stats::coef(object$model, ...)
}


#' @export
summary.fit_logit <- function(object, ...) {
  base::summary(object$model, ...)
}
