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

#' @export
#' @method plot kde
plot.kde <- function(x, y = NULL, ...) {
  plot(x$ud)
}
