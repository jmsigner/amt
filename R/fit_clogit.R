#' Fit a conditional logistic regression
#'
#' This function is a wrapper around `stats::glm`, making it usable in a piped workflow.
#'
#' When including step length and log(step length) in the model and later parameter adjustment is desired, step length must be called `sl_` and log(step length) either `log_sl_` when precalculated, or `log(sl_)` when it is calculated within the formula.
#'
#' @param data The data.
#' @param formula The formula
#' @param more Optional list returned in the output.
#' @param summary_only `[logical(1)=FALSE]` \cr If `TRUE` only a `broom::tidy` summary of the model is returned.
#' @param ... Addtional arguments, passed to `survival::clogit`.
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

