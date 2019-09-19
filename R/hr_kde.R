#' @rdname hr
#' @export
hr_kde <- function(x, ...) {
  UseMethod("hr_kde", x)
}

#' @export
#' @rdname hr
hr_kde.track_xy <- function(x, h = hr_kde_ref(x), trast = raster(as_sp(x), nrow = 100, ncol = 100), ...) {

  # ---------------------------------------------------------------------------- #
  # Check bandwidth
  if (!is.numeric(h)) {
    stop("hr_kde: bandwidth should be numeric")
  } else if (length(h) > 2) {
    warning("hr_kde: h only first 2 elements used")
    h <- h <- h[1:2]
  } else if(length(h) < 2) {
    warning("hr_kde: same bandwidth is used in x and y direction")
    h <- h <- rep(h, 2)
  }

  # Estimate kernels
  xrange <- c(raster::xmin(trast), raster::xmax(trast))
  yrange <- c(raster::ymin(trast), raster::ymax(trast))
  rncol <- raster::ncol(trast)
  rnrow <- raster::nrow(trast)

  # Create Raster
  kde <- KernSmooth::bkde2D(as.matrix(x[, c("x_", "y_")]), bandwidth = h,
                            range.x = list(xrange, yrange),
                            gridsize = c(rncol, rnrow))

  # Finish output
  kde <- raster::raster(t(kde$fhat)[nrow(trast):1,],
                        xmn = xrange[1],
                        xmx = xrange[2],
                        ymn = yrange[1],
                        ymx = yrange[2])

  sp::proj4string(kde) <- get_crs(x)
  res <- list(
    h = h,
    ud = kde
  )
  class(res) <- c("kde", "hr", class(res))
  res
}


#' @rdname hr
#' @export
hr_kde_ref <- function(x, ...) {
  UseMethod("hr_kde_ref", x)
}

#' @export
#' @rdname hr
hr_kde_ref.track_xy <- function(x, rescale = "none", ...) {

  if (!rescale %in% c("unitvar", "xvar", "none")) {
    stop("hr_kde_ref: scale: not one of unitvar, xvar or none")
  }

  xs <- x$x_
  ys <- x$y_

  if (rescale == "unitvar") {
    ## standardize x and y by unit variance
    xs <- xs / sd(xs)
    ys <- ys / sd(ys)

  } else if (rescale == "xvar") {
    ## standardize x and y by
    ys <- (ys / sd(ys)) * sd(xs)
  }

  n <- nrow(x)
  h <- sqrt(0.5 * (var(xs) +  var(ys))) * n^(-1/6)
  h <- c(h, h)

  if (rescale == "unitvar") {
    h <- h * c(sd(x$x_), sd(x$y_))
  }
  h
}


#' `hr_kde_pi` wraps `KernSmooth::dpik` to select bandwidth for kernel density estimation the plug-in-the-equation method in two dimensions.
#' This function calcualtes bandwidths for kernel density estimation by wrapping `KernSmooth::dpik`. If `correct = TURE`, the bandwidth is trasformed with power 5/6 to correct for using an univariate implementation for bivariate data (Gitzen et. al 2006).
#' @param correct Logical scalar that indicates whether or not the estimate should be correct for the two dimensional case.
#' @return The bandwidth, the standardization method and correction.
#' @seealso \code{KernSmooth::dpik}
#' @export
#' @rdname hr
#' @references Gitzen, R. A., Millspaugh, J. J., & Kernohan, B. J. (2006). Bandwidth selection for fixed-kernel analysis of animal utilization distributions. _Journal of Wildlife Management_, 70(5), 1334-1344.
hr_kde_pi <- function(x, ...) {
  UseMethod("hr_kde_pi", x)
}

#' @export
#' @rdname hr
hr_kde_pi.track_xy <- function(x, rescale = "none", correct = TRUE, ...) {


  if (!rescale %in% c("unitvar", "xvar", "none")) {
    stop("rhrHpi: scale: not one of unitvar, xvar or none")
  }

  xs <- dplyr::pull(x, "x_")
  ys <- dplyr::pull(x, "y_")

  if (rescale == "unitvar") {
    # standardize x and y by unit variance
    xs <- xs / sd(xs)
    ys <- ys / sd(ys)

  } else if (rescale == "xvar") {
    # standardize x and y by
    ys <- (ys / sd(ys)) * sd(xs)
  }

  hx <- KernSmooth::dpik(xs, ...)
  hy <- KernSmooth::dpik(ys, ...)

  h <- c(hx, hy)

  if (rescale == "unitvar") {
    h <- KernSmooth::dpik(xs, ...)
    h <- h * c(sd(x[, 1]), sd(x[, 2]))
  }

  # Gitzen et al. 2006 suggested that if bandwidth is estimated for each coordinate seperately, to correct it x^(5/6)
  if (correct) {
    h <- h^(5/6)
  }

  h
}

#' Use least square cross validation (lscv) to estimate bandwidth for kernel home-range estimation.

#' @param range numeric vector with different candidate h values.
#' @param which_min A character indicating if the \code{global} or \code{local} minimum should be searched for.

#' @details `hr_kde_lscv` calcualtes least square cross validation bandwidth. This implementation is based on Seaman and Powell (1996).  If \code{whichMin} is \code{"global"} the global minimum is returned, else the local minimum with the largest candidate bandwidth is returned.

#' @return \code{vector} of length two
#' @export
#' @rdname hr
#' @references Seaman, D. E., & Powell, R. A. (1996). An evaluation of the accuracy of kernel density estimators for home range analysis. _Ecology, 77(7)_, 2075-2085.
#'

hr_kde_lscv <- function(
  x,
  range= do.call(seq, as.list(c(hr_kde_ref(x) * c(0.1, 2), length.out=100))),
  which_min = "global", rescale = "none",
  trast = raster(as_sp(x), nrow = 100, ncol = 100)) {

  if (!rescale %in% c("unitvar", "xvar", "none")) {
    stop("scale: not one of unit, sd or none")
  }

  xs <- dplyr::pull(x, "x_")
  ys <- dplyr::pull(x, "y_")

  if (rescale == "unitvar") {
    ## standardize x and y by unit variance
    xs <- xs / sd(xs)
    ys <- ys / sd(ys)

  } else if (rescale == "xvar") {
    ## standardize x and y by
    ys <- (ys / sd(ys)) * sd(xs)
  }


  ## reference bandwidth
  converged <- TRUE
  res <- lscv(cbind(xs, ys), range)

  if (which_min == "global") {
    h <- range[which.min(res)]
  } else {
    h <- range[max(local_minima(res))]
  }

  ## Did h converge?
  if (range[1] == h | range[length(range)] == h) {
    converged <- FALSE
    warning("hr_kde_lscv: lscv did not converge.")
  }

  ## prepare return
  if (rescale == "unitvar") {
    h <- h * c(sd(x[, 1]), sd(x[, 2]))
  } else {
    h <- c(h, h)
  }
  list(h = h, converged = converged,
       res = res, which_min = which_min, rescale = rescale, range = range)
}

## Helper function from: http://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
local_minima <- function(x) {
  ## Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(.Machine$integer.max, x)) < 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

lscv <- function(x, hs) {
  n <- nrow(x)
  f <- sp::spDists(x)
  f <- f[lower.tri(f)]
  sapply(hs, function(h) {
    out <- sum(exp(-f^2 / (4 * h^2)) - 4 * exp(-f^2 / (2 * h^2)))
    1.0 / (pi * h^2 * n) + (2 * out -3 * n)/(pi * 4. * h^2 * n^2);
  })
}

#' @export
#' @method plot kde
plot.kde <- function(x, y = NULL, ...) {
  plot(x$ud)
}
