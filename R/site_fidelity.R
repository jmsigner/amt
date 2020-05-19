#' Test for site fidelity of animal movement.
#'
#' Calculates two indices (mean sequared displacement and linearity) to test for site fidelity. Significance testing is done by permuting step lengths and drawing turning angles from a uniform distribution.
#'
#' @param x A track
#' @param n Numeric scalar. The number of simulated trajectories.
#' @param alpha Numeric scalar. The alpha value used for the bootstrapping.
#' @export
#' @return A list of length 4. `msd.dat` and `li.dat`` is the mean square distance and linearity for the real date. `msd.sim` and `li.sim`` are the mean square distances and linearities for the simulated trajectories.
#'
#' @references Spencer, S. R., Cameron, G. N., & Swihart, R. K. (1990). Operationally defining home range: temporal dependence exhibited by hispid cotton rats. Ecology, 1817-1822.
#' @examples
#' # Simulate a random walk.
#' set.seed(123)
#' a <- rhrRW(n = 1000)
#' \dontrun{
#' plot(a)
#' }
#'
#' # Calcualte site fidelity
#' sf <- rhrSiteFidelity(a, n = 200)
#'
#' # For MSD and LI the observed data do not differ significantely from random permutations.
#' \dontrun{
#' plot(sf)
#' }
#'
#' # Simulate trajectory as Ornstein-Uhlenbeck process
#' a <- rhrOU(n = 10000, A = matrix(c(0.1, 0, 0, 0.1), 2))
#' plot(a)
#' sf <- rhrSiteFidelity(a, n = 200)
#'
#' # For MSD and LI the observed data differ significantely from random permutations.
#' \dontrun{plot(sf)}
#'
#' ## real data
#' data(datSH)
#' res <- rhrSiteFidelity(datSH[, 2:3], n = 200)
#' \dontrun{
#' plot(res)
#' }

site_fidelty <- function(x, n = 100, alpha = 0.05) {


  # Some argument checking
  checkmate::assert_numeric(n, lower = 1, len = 1)
  checkmate::assert_numeric(alpha, lower = 0, upper = 1, len = 1)

  ## simulate n random walks
  a <- replicate(n, rhrPRW(x, y), simplify=FALSE)

  ## msd
  msdDat <- msd(x, y)
  msdSim <- sapply(a, function(x) rhrMSD2(x[, 1], x[, 2]))

  ## li
  liDat <- li(x, y)
  liSim <- sapply(a, function(x) li(x[,1], x[,2]))

  ## CI
  msdCI <- quantile(msdSim, probs=c(alpha / 2, 1 - alpha / 2))
  liCI <- quantile(liSim, probs=c(alpha / 2, 1 - alpha / 2))


  res <- list(msdDat=msdDat, liDat=liDat, msdSim=msdSim, liSim=liSim,
              msdCI=msdCI, liCI=liCI)

  res <- structure(res, class=c("RhrSiteFidelity", "list"))
  invisible(res)
}


msd <- function(x) {
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

  res <- matrix(nrow = n, ncol = 2)
  res[1, ] <- c(x$x1_[1], x$y1_[1])

  for (i in 1:(n-1)) {
    res[i + 1, 1] = res[i, 1] + cosrA[i] * d[i]
    res[i + 1, 2] = res[i, 2] + sinrA[i] * d[i]
  }
  res
}

