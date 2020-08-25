#' Test for site fidelity of animal movement.
#'
#' Calculates two indices (mean squared displacement and linearity) to test for site fidelity. Significance testing is done by permuting step lengths and drawing turning angles from a uniform distribution.
#'
#' @param x A track
#' @param n Numeric scalar. The number of simulated trajectories.
#' @param alpha Numeric scalar. The alpha value used for the bootstrapping.
#' @param ... None implemented
#' @export
#' @return A list of length 4. `msd_dat` and `li_dat` is the mean square distance and linearity for the real date. `msd_sim` and `li_sim`` are the mean square distances and linearities for the simulated trajectories.
#' @name site_fidelity
#' @references Spencer, S. R., Cameron, G. N., & Swihart, R. K. (1990). Operationally defining home range: temporal dependence exhibited by hispid cotton rats. Ecology, 1817-1822.
#' @examples
#' # real data
#' \dontrun{
#' data(deer)
#' ds <- deer %>% steps_by_burst()
#' site_fidelity(ds)
#' }

site_fidelty <- function(x, ...) {
  UseMethod("site_fidelity", x)
}


#' @rdname site_fidelity
#' @export
site_fidelty.steps_xy <- function(x, n = 100, alpha = 0.05, ...) {

  # Some argument checking
  checkmate::assert_numeric(n, lower = 1, len = 1)
  checkmate::assert_numeric(alpha, lower = 0, upper = 1, len = 1)

  ## simulate n random walks
  a <- replicate(n, permute_steps(x), simplify=FALSE)

  ## msd
  msd_dat <- msd_sf(x)
  msd_sim <- sapply(a, msd_sf)

  ## li
  li_dat <- li(x)
  li_sim <- sapply(a, li)

  ## CI
  msd_ci <- quantile(msd_sim, probs=c(alpha / 2, 1 - alpha / 2))
  li_ci <- quantile(li_sim, probs=c(alpha / 2, 1 - alpha / 2))


  res <-
    list(
      msd_dat = msd_dat,
      li_dat = li_dat,
      msd_sim = msd_sim,
      li_sim = li_sim,
      msd_ci = msd_ci,
      li_ci = li_ci
    )
  invisible(res)
}


msd_sf <- function(x) {
  mx <- mean(x$x1_)
  my <- mean(x$y1_)
  mean((x$x1_ - mx)^2 + (x$y1_ - my)^2)
}



## function for later
li <- function(x) {
  line_distance   <- sqrt((x$x1_[1] - x$x2_[nrow(x)])^2 + (x$y1_[1] - x$y2_[nrow(x)])^2)
  walked_distance <- sum(x$sl_)
  return(line_distance / walked_distance)
}

permute_steps <- function(x) {
  d <- sample(x$sl_)
  n <- length(d)
  a <- runif(n, 0, 2 * pi)
  sinrA <- sin(a)
  cosrA <- cos(a)

  res <- data.frame(x1_ = NA, y1_ = NA, x2_ = NA, y2_ = NA, sl_ = NA)
  xs <- x$x1_[1]
  ys <- x$y1_[1]

  # in steps
  for (i in 1:n) {
    xe <- xs + cosrA[i] * d[i]
    ye <- ys + sinrA[i] * d[i]
    res[i, ] <- list(xs, ys, xe, ye, d[i])
    xs <- xe
    ys <- ye
  }
  res
}

