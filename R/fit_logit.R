#' Fit logistic regression
#'
#' This function is a wrapper around `stats::glm` for piped workflows.
#' @param data `[data.frame]` \cr The data used to fit a model.
#' @param formula `[formula]` \cr The model formula.
#' @param ... Further arguments passed to `stats::glm`.
#' @name fit_logit
#' @export
fit_logit <- function(data, formula, ...) {
  m <- stats::glm(formula, data = data, family = stats::binomial(link = "logit"), ...)
  m <- list(model = m)
  class(m) <- c("fit_logit", "glm", class(m))
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

#' @rdname fit_logit
#' @export
fit_rsf <- fit_logit
