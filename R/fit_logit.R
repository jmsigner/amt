#' @export
fit_logit <- function(x, ...) {
  UseMethod("fit_logit", x)
}

#' @export
fit_logit.random_points <- function(data, formula, more = NULL, ...) {
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
