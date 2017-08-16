#' Adjust parameters
#'
#' Functions aiding parameter adjustment after fitting an integrated step
#' selection function (iSSF).
#'
#' Currently the shape and scale parameter of a Gamma distribution and the
#' concentration parameter (=kappa) of a von Mises distribution can be adapted.
#' When fitting a integrated step selection model, the tentative shape parameter
#' of the Gamma distribution of the step length can be adjusted with the
#' coefficients of the log of step lengths, the scale of the Gamma distribution
#' can be adjusted with the step lengths, and the kappa parameter of the von
#' Mises distribution can be adjusted with the coefficient for the cosine of the
#' turn angle.
#'
#' @param tentative `[numeric]` \cr The tentative parameter estimate.
#' @param modifier `[numeric=0]` \cr The modifier to adjust the tentative
#'   estimate.
#' @name adjust_param
#' @references
#' \insertRef{avgar2016}{amt}
#' @export
adjust_shape <- function(tentative, modifier = 0) {
  tentative + modifier
}

#' @export
#' @rdname adjust_param
adjust_scale <- function(tentative, modifier = 0) {
  1 / ((1 / tentative) - modifier)
}

#' @export
#' @rdname adjust_param
adjust_kappa <- function(tentative, modifier = 0) {
  tentative + modifier
}

# obsolete
adjust_params <- function(x, coef_names = c(step_length = "sl_", log_step_length = "log_sl_"), ...) {
  if (x$sl_$distname == "gamma") {
    params <- sl_params(x$sl_, ...)
    coef_n <- names(coef(x))

    # following Avgar et al. 2016
    # see background/shape&scale.pdf from Tal

    scale <- params["scale", c(1, 3:4)] # 1: est; 3:4 are the CI
    scale_modifier <- if ("step_length" %in% names(coef_names)) {
      coef(x)[coef_names["step_length"]]
    } else {
      0
    }

    shape <- params["shape", c(1, 3:4)]
    shape_modifier <- if ("step_length" %in% names(coef_names)) {
      coef(x)[coef_names["log_step_length"]]
    } else {
      0
    }

    rbind(
      shape = shape + shape_modifier,
      scale = 1 / ((1 / scale) - scale_modifier)
    )

  } else {
    stop("Not yet implemented")
  }
}
