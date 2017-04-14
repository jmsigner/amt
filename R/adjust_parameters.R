#' Adjust Movement Parameters.
#'
#' Adjusting movement based parameters, based on Avgar et al. 2016.
#'
#' @param x A fitted conditional logistic regression
#'
#' @export
adjust_params <- function(x) {
  if (x$sl_$distname == "gamma") {
    params <- sl_params(x$sl_)
    coef_n <- names(coef(x))

    if (any(grepl("sl_", coef_n)) & any(grepl("log\\(sl_\\)", coef_n))) {
      # following Avgar et al. 2016
      # see background/shape&scale.pdf from Tal
      c(
        params["shape"] + coef(x)["log(sl_)"],
        1 / ((1 / params["scale"]) - coef(x)["sl_"])
      )
    } else {
      stop("Parameter adjustment requested, but 'sl_' and 'log(sl_)' were not included in the model.")
    }
  } else {
    stop("Not yet implemented")
  }
}
