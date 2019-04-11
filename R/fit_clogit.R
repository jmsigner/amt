#' Fit a conditional logistic regression
#'
#' This function is a wrapper around `survival::clogit`, making it usable in a piped workflow.
#'
#' @param data `[data.frame]` \cr The data used to fit a model.
#' @param formula `[formula]` \cr The model formula.
#' @param more `[list]` \cr Optional list that is passed on the output.
#' @param summary_only `[logical(1)=FALSE]` \cr If `TRUE` only a `broom::tidy` summary of the model is returned.
#' @param ... Additional arguments, passed to `survival::clogit`.
#' @name fit_clogit
#' @export

fit_clogit <- function(data, formula, more = NULL, summary_only = FALSE, ...) {
  m <- survival::clogit(formula, data = data, ...)

  if (summary_only) {
    m <- list(model = broom::tidy(m),
              sl_ = attributes(data)$sl_,
              ta_ = attributes(data)$ta_,
              more = more)
    class(m) <- c("fit_clogit_summary_only", "fit_clogit", class(m))
  } else {
    m <- list(model = m,
              sl_ = attributes(data)$sl_,
              ta_ = attributes(data)$ta_,
              more = more)
    class(m) <- c("fit_clogit", class(m))
  }
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

#' @export
AIC.fit_clogit <- function(object, ...) {
  stats::AIC(object$model, ...)
}


#' @rdname fit_clogit
#' @export
fit_ssf <- fit_clogit

#' @rdname fit_clogit
#' @export
fit_issf <- fit_clogit
