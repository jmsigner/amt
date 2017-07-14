simulate_ssf <- function(scale, shape, quant = 0.99, sel_coef, resources, n, start) {

  ###
  if (FALSE) {
    scale = 2
    shape = 2
    sel_coef = c(-0.05, 2)
    n = 100
    start = c(100, 100)
    resources = rsc
    quant = 0.99
    start = c(100, 100)
  }
  ###


  # mk
  dist <- ceiling(qgamma(quant, scale = scale, shape = shape))
  mk <- mk_base(dist, raster::res(resources)[1])
  mk <- mk_gamma(mk, shape = shape, scale = scale)

  # hk
  hk <- raster::raster(resources)
  hk <- raster::setValues(hk, 0)
  for (i in 1:length(sel_coef)) {
    hk <- hk + resources[[i]] * sel_coef[i]
  }
  hk <- exp(hk)

  nc <- ncol(hk)
  nr <- nrow(hk)

  mk1 <- raster::rasterToPoints(mk)
  mk1[, c("x", "y")] <- mk1[, c("x", "y")] / raster::res(mk)[1]

  hk1 <- as.vector(m <- t(raster::as.matrix(hk)))
  y <- as.vector(col(m))
  x <- as.vector(row(m))
  hk1 <- cbind(cell = nc * (y - 1) + x, hk1)

  # start cell
  start1 <- nc * (nr - start[2]) + start[1]

  s <- cpp_simulate_ssf(n, start1, nc, nr, mk1, hk1)

  rr <- raster::res(resources)
  xy <- cbind(x = s %% nc + 0.5 * rr, y = s %/% nr + 0.5 * rr)
  return(xy)


}
