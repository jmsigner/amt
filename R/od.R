#' Occurence Distribution
#'
#' `od` is a wrapper around `ctmm::occurrence`. See `help(ctmm::occurrence)` for more details. `rolling_od` estimates occurences distributions for a subset of a track.
#'
#'
#'
#'
#' @template track_xyt
#' @param trast `[RasterLayer]` \cr A template raster for the extent and resolution of the result.
#' @param model `[character(1)="bm"]{"bm","ou","ouf","auto"}` \cr The autocorrelation model that should be fit to the data. `bm` corresponds to Brownian motion, `ou` to an Ornstein-Uhlenbeck process, `ouf` to an Ornstein-Uhlenbeck forage process. The option `auto` fits all three processes from above and then performs AIC-based model selection.
#'   local convex hulls.
#' @param res.space `[numeric(1)=10]` \cr Number of grid point along each axis, relative to the average diffusion (per median timestep) from a stationary point. See also `help(ctmm::occurrence)`.
#' @param res.time `[numeric(1)=10]` \cr Number of temproal grid points per median timestep.
#' @param n.points `[numeric(1)=5]` \cr This argument is only relevant for `rolling_od` and specifies the window size for the od estimation.
#' @template dots_none
#' @references Fleming, C. H., Fagan, W. F., Mueller, T., Olson, K. A., Leimgruber, P., & Calabrese, J. M. (2016). Estimating where and how animals travel: an optimal framework for path reconstruction from autocorrelated tracking data. Ecology.
#' @name od
#' @examples
#'
#'

#' @export
rolling_od <- function(x, ...) {
  UseMethod("rolling_od", x)
}

#' @export
#' @rdname od
rolling_od.track_xyt <- function(x, trast, model = "bm", res.space = 10, res.time = 10, n.points = 5, ...) {
  res <- list()
  pb <- progress::progress_bar$new(total = (nrow(x) - (n.points + 1)))
  for (i in (n.points + 1):nrow(x)) {
    pb$tick()
    j <- i - n.points
    res[[j]] <- od(x[j:i, ], trast = trast, model = model, res.space = res.space, res.time = res.time)
  }
  raster::stack(res)
}


# ? would it be easier for users to just input an extent and resolution for the template raster instead of a template raster??
#' @export
#' @rdname od
od <- function(x, ...) {
  UseMethod("rolling_od", x)
}

#' @export
#' @rdname od
od.track_xyt <- function(x, trast, model = "auto", res.space = 10, res.time = 10) {

  if (!has_crs(x)) {
    stop("x, needs a cooridnate reference system (crs).")
  }

  if (!model %in% c("bm", "ou", "ouf", "auto")) {
    stop("Unknown model selected.")
  }

  m <- move::move(x=x$x_, y=x$y_,
                  time = x$t_, proj = amt::get_crs(x),
                  data = data.frame(individual.local.identifier = rep("1", nrow(x))))

  suppressWarnings(suppressMessages(dat <- ctmm::as.telemetry(m)))

  g <- ctmm::ctmm.guess(dat, interactive = FALSE)

  mod <- if (tolower(model) == "auto") {
    ms <- list(
      m_bm  = ctmm::ctmm.fit(dat, ctmm::ctmm(tau = Inf)),
      m_ou  = ctmm::ctmm.fit(dat, ctmm::ctmm(tau = g$tau[1])),
      m_ouf = ctmm::ctmm.fit(dat, ctmm::ctmm(tau = g$tau[1:2]))
    )
    aics <- sapply(ms, "[[", "AIC") ## !! removed "which.min"
    model <- c("bm", "ou", "ouf")[which.min(aics)] ## !! maybe choose different variable name otherwise same as function input "model"?
    # maybe make a rule that only more complex (ou, ouf) models are selected if delta AIC is larger than 2 or so
    # BTW, summary(ms) gives a table with delta AICs
    ctmm.fit(dat, ms[[which.min(aics)]])
  } else {
    if (model == "bm") {
      m_bm = ctmm::ctmm.fit(dat, ctmm::ctmm(tau = Inf))
    } else if (model == "ou") {
      m_ou = ctmm::ctmm.fit(dat, ctmm::ctmm(tau = g$tau[1]))
    } else if (model == "ouf") {
      m_ouf = ctmm::ctmm.fit(dat, ctmm::ctmm(tau = g$tau[1:2]))
    } else {
      stop("Unknown model selected")
    }
  }

  krige <- ctmm::occurrence(dat, CTMM = mod, res.space = res.space, res.time = res.time)

  r <- 1 - ctmm::raster(krige, DF = "CDF")
  #r <- raster::projectRaster(r, crs = amt::get_crs(x))
  ## !! alternative with option res: (was necessary for vole utm data)
  r <- raster::projectRaster(r, crs = amt::get_crs(x), res = krige$dr)
  r <- raster::resample(r, trast)
  r <- raster::reclassify(r, cbind(NA, 0))
  attr(r, "model") <- model
  r
}
