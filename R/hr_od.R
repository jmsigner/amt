#' @rdname hr
#' @export
#' @references Fleming, C. H., Fagan, W. F., Mueller, T., Olson, K. A., Leimgruber, P., & Calabrese, J. M. (2016). Estimating where and how animals travel: an optimal framework for path reconstruction from autocorrelated tracking data. Ecology, 97(3), 576-582.
#' @examples
#' # od
#' \dontrun{
#' data(deer)
#' ud1 <- hr_od(deer) # uses an iid ctmm
#' ud2 <- hr_akde(deer, model = fit_ctmm(deer, "ou")) # uses an OU ctmm
#' }
hr_od <- function(x, ...) {
  UseMethod("hr_od", x)
}


#' @export
#' @rdname hr
hr_od.track_xyt <- function(x, model = fit_ctmm(x, "iid"), keep.data = TRUE,
                              trast = make_trast(x), levels = 0.95, ...) {

  est <- od(x, model = model, trast = trast, ...)

  res <- list(ud = est, model = model, levels = levels, trast = trast,
              estimator = "od",
              crs = get_crs(x),
              data = if (keep.data) x else NULL)
  class(res) <- c("od", "hr_prob", "hr", class(res))
  res
}

