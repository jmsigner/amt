# Functions ---------------------------------------------------------------

get_angle <- function(xy, dir = 0, take_cosine = TRUE) {
  ta <- atan2(xy[, 2], xy[, 1])
  ta <- ta - (pi/2) + dir
  ta <- ifelse(ta < -pi, ta + 2 * pi, ta)
  ta <- ifelse(ta > pi, ta - 2 * pi, ta)
  if (take_cosine) cos(ta) else ta
}

prep_and_check_simulations_to_rcpp <- function(
  formula, coefs, habitat, other.vars = NULL, start, max.dist,
  init.dir = amt::as_rad(45), standardize = TRUE, stop, n = 1
) {

  checkmate::assert_formula(formula)
  checkmate::assert_named(coefs)
  checkmate::assert_numeric(start, len = 2)
  checkmate::assert_numeric(max.dist, lower = 0, len = 1)

  # check that start is within landscape
  if (!(start[1] >= raster::xmin(habitat) & start[1] <= raster::xmax(habitat) &
    start[2] >= raster::ymin(habitat) & start[2] <= raster::ymax(habitat))) {
    stop("Start position is not within landscape")
  }

  # prep terms
  trms <- attr(terms(formula), "term.labels")
  all_terms <- unique(do.call(c, strsplit(trms, ":")))

  data_names <- c("sl_", "log_sl_", "cos_ta_")
  other.vars_indicator <- c(0, 0)

  # habitat
  if (is.null(habitat)) {
    stop("habitat mask is needed.")
  }

  hab_end <- sapply(strsplit(all_terms[grepl(pattern = "_end$", all_terms)],
                             "_end"), "[", 1)
  hab_start <- sapply(strsplit(all_terms[grepl(pattern = "_start$", all_terms)],
                               "_start"), "[", 1)
  data_names <- c(data_names, paste0(names(habitat), "_start"),
                  paste0(names(habitat), "_end"))
  res <- raster::res(habitat)[1]

  # other
  if (!is.null(other.vars)) {
    data_names <- c(data_names, colnames(other.vars))
    other.vars_indicator[2] <- 1
    if (!is(other.vars, "matrix")) {
      other.vars <- as.matrix(other.vars)
    }
  } else {
    other.vars <- matrix(NA, nrow = n, 1)
  }

  first_order <- sapply(strsplit(trms, ":"), "[", 1)
  int_with <- sapply(strsplit(trms, ":"), "[", 2)

  if (!all(first_order %in% data_names)) {
    stop(paste0(
      "Some first order terms (",
      paste(first_order[!first_order %in% data_names], collapse = ",") ,
      ") not in coefficients. Did you forget *term*_start or *term*_end?" ))
  }

  first_order_terms <- match(first_order, data_names)
  second_order_terms <- match(int_with, data_names, nomatch = 0)

  # Dispersal kernel stencil, in cells
  dist_in_cells <- ceiling(max.dist / res)

  dk <- expand.grid(x = -dist_in_cells:dist_in_cells,
                    y = -dist_in_cells:dist_in_cells)
  dk[, "sl_"] <- sqrt(dk[, "x"]^2 + dk[, "y"]^2) * res
  dk <- dk[dk[, "sl_"] < dist_in_cells * res, ]
  dk[, "log_sl_"] <- log(dk[, "sl_"])
  dk[, "cos_ta_"] <- get_angle_cpp2(as.matrix(dk[, 1:2]), dir = init.dir)
  dk <- as.matrix(dk)


  nc <- ncol(habitat)
  nr <- nrow(habitat)
  hab1 <- (raster::getValues(habitat))
  other.vars_indicator[1] <- 1

  # Start as cells
  start[1] <- raster::colFromX(habitat, start[1]) - 1
  start[2] <- nr - raster::rowFromY(habitat, start[2]) + 1

  list(
    start = start,
    nc = nc, nr = nr,
    res = res,
    dk = dk, coefs = coefs,
    init_dir = init.dir, standardize = standardize,
    first_order_terms = first_order_terms - 1,
    second_order_terms = second_order_terms - 1,
    hab = hab1,
    other_covars = other.vars,
    other_covars_indicator = other.vars_indicator,
    data_names = data_names,
    stop = stop
  )
}

#' Create a dispersal kernel
#'
#' @param formula `[formula]` \cr The formula for the dispersal kernel.
#' @param coefs `[named numeric]{>1}` \cr Coefficients for the terms in the formula. Names of the coefficients must match the name of the terms.
#' @param habitat `[RasterLayer]` \cr The habitat matrix / landscape.
#' @param other.vars `[data.frame = NULL]` \cr Possible other covariates.
#' @param start `[numeric(2)]` \cr Coordinates of the start position.
#' @param max.dist `[numeric(1)]` \cr The maximum distance of the dispersal kernel.
#' @param init.dir `[numeric(1)]` \cr The initial direction in rad.
#' @param standardize `[logical(1) = TRUE]` \cr Should the result be standardized.
#' @param raster `[logical(1) = TRUE]` \cr Should a `RasterLayer` be returned.
#' @param stop `[integer(1)=1]{0,1}` \cr What happens when the animal steps out of the landscape.
#'
#' @export
#'
dispersal_kernel <- function(
  formula, coefs, habitat = NULL, other.vars = NULL, start, max.dist,
  init.dir = amt::as_rad(45), standardize = TRUE, raster = TRUE, stop = 0) {


  # Prepare params
  p <- prep_and_check_simulations_to_rcpp(
    formula, coefs, habitat, other.vars, start, max.dist,
    init.dir, standardize, stop
  )

  k <- dispersal_kernel_cpp(
    cur_x = p$start[1], cur_y = p$start[2], nc = p$nc, nr = p$nr,
    dk = p$dk, coefs = p$coefs,
    standardize = p$standardize,
    first_order_terms = p$first_order_terms,
    second_order_terms = p$second_order_terms,
    hab = p$hab,
    other_covars = p$other_covars,
    other_covars_indicator = p$other_covars_indicator,
    stop = p$stop
  )

  # Return
  kernel <- if (raster) {
    raster::rasterFromXYZ(cbind(
      k[, 1] * p$res,
      k[, 2] * p$res,
      k[, 3]))
  } else {
    cbind(
      x = k[, 1] * p$res + start[1],
      y = k[, 2] * p$res + start[2],
      sel = k[, 3]
    )
  }

  out <- list(
    formula = formula,
    coefs = coefs,
    habitat = habitat,
    other.vars = other.vars,
    start = start,
    max.dist = max.dist,
    init.dir = init.dir,
    standardize = standardize,
    raster = raster,
    stop = stop,
    prep_dk = p,
    dispersal_kernel = kernel
  )
  class(out) <- c("dispersal_kernel", class(out))
  out
}


#' Simulate a trajectory
#'
#' @param obj A dispersal kernel.
#' @param n Number of time steps.
#' @param other.vars Other covariates (for each time step).
#'
#' @export
#'
simulate_xy <- function(obj, n = 100, other.vars = NULL) {

   if (!is(obj, "dispersal_kernel")) {
     stop("obj is no dispersal kernel")
   }
   p <- obj$prep_dk
   if (is.null(obj$other.vars)) {
     p$other_covars <- matrix(NA, nrow = n, ncol = 1)
   }

   x <- p$start[1]
   y <- p$start[2]

   out <- data.frame(
     x1 = rep(NA, n),
     y1 = NA,
     x2 = NA,
     y2 = NA,
     sl_ = NA,
     ta_ = NA
   )

   dk <- p$dk

   for (i in 1:n) {
     out$x1[i] <- x
     out$y1[i] <- y
     k <- dispersal_kernel_cpp(
       cur_x = x, cur_y = y, nc = p$nc, nr = p$nr,
       dk = p$dk, coefs = p$coefs,
       standardize = p$standardize,
       first_order_terms = p$first_order_terms,
       second_order_terms = p$second_order_terms,
       hab = p$hab,
       other_covars = p$other_covars,
       other_covars_indicator = p$other_covars_indicator,
       stop = p$stop
     )

     next_pixel <- sample.int(nrow(p$dk), size = 1, prob = k[, 3])
     x <- k[next_pixel, 1]
     y <- k[next_pixel, 2]

     ta <- atan2_north_cpp(y -  out[i, "y1"], x - out[i, "x1"])
     dk[, "cos_ta_"] <- get_angle_cpp2(dk[, 1:2], dir = ta)
     out$x2[i] <- x
     out$y2[i] <- y
     out$sl_[i] <- p$dk[next_pixel, "sl_"]
     out$ta_[i] <- ta
   }
   out
 }

#' Simulate a UD from a dispersal kernel
#'
#' @param obj A dispersal kernel
#' @param n Number of time steps
#' @param other.vars other covariates for each time step.
#'
#' @export
#'
simulate_ud_from_dk <- function(obj, n = 1e3, other.vars = NULL) {
   if (!is(obj, "dispersal_kernel")) {
     stop("obj is no dispersal kernel")
   }
   p <- obj$prep_dk
   if (is.null(obj$other.vars)) {
     p$other_covars <- matrix(NA, nrow = n, ncol = 1)
   }
   ud <- obj$habitat
   x <- p$start[1]
   y <- p$start[2]
   dk <- p$dk

   cells <- rep(NA, n)

   for (i in 1:n) {
     k <- dispersal_kernel_cpp(
       cur_x = x, cur_y = y, nc = p$nc, nr = p$nr,
       dk = p$dk, coefs = p$coefs,
       standardize = p$standardize,
       first_order_terms = p$first_order_terms,
       second_order_terms = p$second_order_terms,
       hab = p$hab,
       other_covars = p$other_covars,
       other_covars_indicator = p$other_covars_indicator,
       stop = p$stop
     )

     next_pixel <- sample.int(nrow(p$dk), size = 1, prob = k[, 3])
     xn <- k[next_pixel, 1]
     yn <- k[next_pixel, 2]

     cells[i] <- raster::cellFromXY(ud, cbind(xn, yn))
     ta <- atan2_north_cpp(yn -  y, xn - x)
     dk[, "cos_ta_"] <- get_angle_cpp2(dk[, 1:2], dir = ta)
     x <- xn
     y <- yn
   }

   cl <- table(cells)
   ud[as.numeric(names(cl))] <- cl
   ud
 }


# #### OLD
# simulate_xy <- function(x, n = 100, other.vars = NULL) {
#
#   if (!is(x, "dispersal_kernel")) {
#     stop("x is no dispersal kernel")
#   }
#   p <- x$prep_dk
#   if (is.null(x$other.vars)) {
#     p$other_covars <- matrix(NA, nrow = n, ncol = 1)
#   }
#
#   k <- simulate_track(
#     cur_x = p$start[1], cur_y = p$start[2], nc = p$nc, nr = p$nr,
#     dk = p$dk, coefs = p$coefs,
#     standardize = p$standardize,
#     first_order_terms = p$first_order_terms,
#     second_order_terms = p$second_order_terms,
#     hab = p$hab,
#     other_covars = p$other_covars,
#     other_covars_indicator = p$other_covars_indicator,
#     stop = p$stop, n = n
#   )
#
#   tibble::tibble(
#     x1 = k[, 1] * p$res,
#     y1 = k[, 2] * p$res,
#     x2 = k[, 3] * p$res,
#     y2 = k[, 4] * p$res,
#     ta_ = k[, 5],
#     sl_ = k[, 6] * p$res
#   )
#
# }
#
#  simulate_xy_R <- function(obj, n = 100, other.vars = NULL) {
#
#    if (!is(obj, "dispersal_kernel")) {
#      stop("obj is no dispersal kernel")
#    }
#    p <- obj$prep_dk
#    if (is.null(obj$other.vars)) {
#      p$other_covars <- matrix(NA, nrow = n, ncol = 1)
#    }
#
#    x <- p$start[1]
#    y <- p$start[2]
#
#    out <- data.frame(
#      x1 = rep(NA, n),
#      y1 = NA,
#      x2 = NA,
#      y2 = NA,
#      sl_ = NA,
#      ta_ = NA
#    )
#
#    dk <- p$dk
#
#    for (i in 1:n) {
#
#      out$x1[i] <- x
#      out$y1[i] <- y
#
#      k <- dispersal_kernel(
#        cur_x = x, cur_y = y, nc = p$nc, nr = p$nr,
#        dk = p$dk, coefs = p$coefs,
#        standardize = p$standardize,
#        first_order_terms = p$first_order_terms,
#        second_order_terms = p$second_order_terms,
#        hab = p$hab,
#        other_covars = p$other_covars,
#        other_covars_indicator = p$other_covars_indicator,
#        stop = p$stop
#      )
#
#      next_pixel <- sample.int(nrow(p$dk), size = 1, prob = k[, 3])
#      x <- k[next_pixel, 1]
#      y <- k[next_pixel, 2]
#
#      ###
#      #plot(rasterFromXYZ(k))
#      #points(out[i, "x1"], out[i, "y1"])
#      #points(x, y)
#
#
#      atan2_north <- function(y, x) {
#        ta <- atan2(y, x)
#        ta <- ta - (pi/2)
#        ta <- ifelse(ta < -pi, ta + 2 * pi, ta)
#        ifelse(ta > pi, ta - 2 * pi, ta)
#      }
#
#      ta <- atan2_north(y -  out[i, "y1"], x - out[i, "x1"])
#      dk[, "cos_ta_"] <- get_angle(dk[, 1:2], dir = ta, FALSE)
#      # plot(rasterFromXYZ(dk[, c(1:2, 5)]))
#      # head(dk)
#      ###
#
#      out$x2[i] <- x
#      out$y2[i] <- y
#
#      out$sl_[i] <- p$dk[next_pixel, "sl_"]
#      out$ta_[i] <- ta
#    }
#    out
#  }
#
