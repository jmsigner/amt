#' Adjust parameters
#'
#' Functions aiding parameter adjustment after fitting an integrated step
#' selection function (iSSF).
#'
#' Currently the shape and scale parameter of a Gamma distribution and the
#' concentration parameter (=kappa) of a von-Mises distribution can be adjusted.
#' When fitting a integrated step selection model, the tentative shape parameter
#' of the Gamma distribution of the step length can be adjusted with the
#' coefficients of the log of step lengths, the scale of the Gamma distribution
#' can be adjusted with the step lengths, and the kappa parameter of the von
#' Mises distribution can be adjusted with the coefficient for the cosine of the
#' turn angle.
#'
#' @examples
#' # Using the deer data set
#' data(deer)
#' data(sh_forest)
#'
#' # first prepare the data and fit a model
#' m1 <- deer %>% steps_by_burst() %>%
#'   random_steps() %>%
#'   extract_covariates(sh_forest) %>%
#'   mutate(sh.forest = factor(sh.forest)) %>%
#'   fit_clogit(case_ ~ sh.forest * log(sl_) + sl_ + strata(step_id_))
#'
#'
#'
#' # Investigate and adjust parameters ---------------------------------------
#'
#' sl_params(m1)['shape', 'est']
#' # adjust shape with the log of the step length
#' sh1 <- adjust_shape(sl_params(m1)['shape', 'est'],
#'              modifier = coef(m1)['log(sl_)'])
#'
#' sl_params(m1)['scale', 'est']
#' # adjust shape with the step length
#' sc1 <- adjust_shape(sl_params(m1)['scale', 'est'],
#'              modifier = coef(m1)['sl_'])
#'
#' # Up to now we have ignored the interaction with forest
#' # this means the above assumes that forest = 2 (= non forest)
#'
#' sl_params(m1)['shape', 'est']
#' # adjust shape with the log of the step length
#' sh2 <- adjust_shape(tentative = sl_params(m1)['shape', 'est'],
#'                     modifier = coef(m1)['log(sl_)'] + coef(m1)['sh.forest2:log(sl_)'])
#'
#' # The modified shape paremeter differ for forest and non forest.
#' # The shape for steps that end in forest are lower.
#' sh1
#' sh2
#'
#' # This can be best seen when plotting the tentative Gamma distribution (black) and
#' # adding lines for Gamma distributions with adjusted shape paremeters
#' # for open areas (red) and forested areas (green).
#' \dontrun{
#' plot_sl(m1)
#' curve(dgamma(x, shape = sh1, scale = sc1), col = "red", add = TRUE, from = 0.1)
#' curve(dgamma(x, shape = sh2, scale = sc1), col = "forestgreen", add = TRUE, from = 0.1)
#' }
#'
#' @param tentative `[numeric]` \cr The tentative parameter estimate.
#' @param modifier `[numeric=0]` \cr The modifier to adjust the tentative
#'   estimate.
#' @name adjust_param
#' @references
#' \insertRef{avgar2016}{amt}
#' @export
adjust_shape <- function(tentative, modifier = 0) {
  adj <- tentative + modifier
  ifelse(adj < 0, 0, adj)
}

#' @export
#' @rdname adjust_param
adjust_scale <- function(tentative, modifier = 0) {
  adj <- 1 / ((1 / tentative) - modifier)
  ifelse(adj < 0, 0, adj)
}

#' @export
#' @rdname adjust_param
adjust_kappa <- function(tentative, modifier = 0) {
  adj <- tentative + modifier
  ifelse(adj < 0, 0, adj)
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
