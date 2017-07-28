#' @rdname fit_clogit
#' @export
adjust_params <- function(x) {
  if (x$sl_$distname == "gamma") {
    params <- sl_params(x$sl_)
    coef_n <- names(coef(x))

    if (any(grepl("sl_", coef_n)) & any(grepl("log\\(sl_\\)|log_sl_", coef_n))) {
      # following Avgar et al. 2016
      # see background/shape&scale.pdf from Tal
      c(
        params["shape"] + if (any(grepl("log_sl_", coef_n))) coef(x)["log_sl_"] else coef(x)["log(sl_)"],
        1 / ((1 / params["scale"]) - coef(x)["sl_"])
      )
    } else {
      stop("Parameter adjustment requested, but 'sl_' and 'log(sl_)' were not included in the model.")
    }
  } else {
    stop("Not yet implemented")
  }
}
