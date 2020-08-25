#' Fit a continuous time movement model with `ctmm`
#'
#' @template track_xyt
#' @param model `[character(1)="bm"]{"iid", "bm","ou","ouf", "auto"}` \cr The autocorrelation model that should be fit to the data. `iid` corresponds to uncorrelated independent data, `bm` to Brownian motion, `ou` to an Ornstein-Uhlenbeck process, `ouf` to an Ornstein-Uhlenbeck forage process. `auto` will use model selection with AICc to find the best model.
#' @param ... Additional parameters passed to `ctmm::ctmm.fit` or `ctmm::ctmm.select` for `model = "auto"`
#'
#' @return A `ctmm` object.
#' @export
#' @references C. H. Fleming, J. M. Calabrese, T. Mueller, K.A. Olson, P. Leimgruber, W. F. Fagan, “From fine-scale foraging to home ranges: A semi-variance approach to identifying movement modes across spatiotemporal scales”, The American Naturalist, 183:5, E154-E167 (2014).
#'
#' @examples
#' data(deer)
#' m1 <- fit_ctmm(deer, "iid")
#' summary(m1)
fit_ctmm <- function(x, model, ...) {

  if (!model %in% c("iid", "bm", "ou", "ouf", "auto")) {
    stop("Unknown model selected.")
  }

  suppressWarnings(suppressMessages(dat <- as_telemetry(x)))

  g <- ctmm::ctmm.guess(dat, interactive = FALSE)

  mod <- if (model == "iid") {
    ctmm::ctmm.fit(dat, ctmm::ctmm(tau = NULL), ...)
  } else if (model == "bm") {
    ctmm::ctmm.fit(dat, ctmm::ctmm(tau = Inf), ...)
  } else if (model == "ou") {
    ctmm::ctmm.fit(dat, ctmm::ctmm(tau = g$tau[1]), ...)
  } else if (model == "ouf") {
    ctmm::ctmm.fit(dat, ctmm::ctmm(tau = g$tau[1:2]), ...)
  } else if (model == "auto") {
    ctmm::ctmm.select(dat, g, ...)
  } else {
    stop("Unknown model selected")
  }
  mod
}
