#' Different Methods to calculate home-range overlaps
#'
#' @param x,y `hr` A home-range estimate
#' @template dots_none
#' @return \code{data.frame} with the isopleth level and area in units of the coordinate reference system.
#' @name hr_overlaps
NULL

#' @rdname hr_overlaps
#' @export
hr_overlap <- function (x, ...) {
  UseMethod("hr_overlap", x )
}

#' @export
hr_overlap.default <- function (x, ...) {
  print("Not implemented for this class.")
}

#' @export
#' @rdname hr_overlaps
hr_overlap.hr <- function(x, y, ...) {

  if (!inherits(y, "hr")) {
    stop("y is no home-range estimate.")
  }
  x <- hr_isopleths(x)
  y <- hr_isopleths(y)
 overlap_base(x, y)
}


#' @rdname hr_overlaps
#' @export
#' @param  consecutive.only `[logical=TRUE]` \cr Should only consecutive overlaps be calculated or all combinations?
#' @param  labels `[character=NULL]` \cr Labels for different instances. If `NULL` (the defualt) numbers will be used.
hr_overlap.list <- function(x, consecutive.only = TRUE, labels = NULL, ...) {

  # check all elements are hr
  if(!all(sapply(x, inherits, "hr"))) {
    stop("Not all x are home-range estimates.")
  }

  if (length(x) < 2) {
    stop("At least two home range estimates are needed")
  }

  nn <- if (is.null(names(x))) 1:length(x) else names(x)

  if (!is.null(labels)) {
    if (length(labels) == length(nn)) {
      nn <- labels
    } else {
      warning("Length of `labels` does not match the number of cases. Using default instead.")

    }
  }

  if (any(nchar(nn) == 0)) {
    stop("Names must be different from ''")
  }

  if (any(duplicated(nn))) {
    stop("Duplicated names are not permitted")
  }

  isos <- lapply(x, hr_isopleths)
  levels <- lapply(isos, function(x) x$level)
  if (!all(sapply(levels[-1], function(x) all(levels[[1]] %in% x)))) {
    stop("Not all levels of first home range est are in the others as well.")
  }

  grid <- if (consecutive.only) {
    tibble(from = nn[-length(nn)], to = nn[-1])
  } else {
    tidyr::expand_grid(from = nn, to = nn) %>%
      dplyr::filter(from != to)
  }

  grid %>%
    dplyr::mutate(overlap = purrr::map2(from, to, function(i, j) {
      overlap_base(isos[[i]], isos[[j]])
    })) %>%
    tidyr::unnest(cols = "overlap")
}

overlap_base <- function(x, y) {

  if(all(x$levels %in% y$levels)) {
    if (length(x$level) == 1) {
      if (sf::st_intersects(x, y, sparse = FALSE)) {
        suppressWarnings(ol <- sf::st_intersection(x, y))
        return(as.numeric(sf::st_area(ol) / sf::st_area(x)))
      } else {
        return(0)
      }
    } else if (length(x$level) > 1) {
      return(
        tibble::tibble(
          level = x$level,
          overlap = sapply(1:nrow(x), function(i) overlap_base(x[i, ], y[i, ])))
        )
    }
  } else {
    stop("Not all levels of x in y.")
  }
}

#' @rdname hr_overlaps
#' @export
hr_ba <- function (x, ...) {
  UseMethod ("hr_ba", x )
}

#' @export
hr_ba.hr_prob <- function(x, y, conditional = 0.95, ...) {


  x_ud <- hr_ud(x)
  y_ud <- hr_ud(y)
  if (conditional < 1) {
    x_ud[] <- ifelse(hr_cud(x)[] <= conditional, x_ud[], 0) / conditional
    y_ud[] <- ifelse(hr_cud(y)[] <= conditional, y_ud[], 0) / conditional
  }

  hr_ba(hr_ud(x), hr_ud(y), conditional, ...)
}

#' @export
hr_ba.RasterLayer <- function(x, y, ...) {

  if (!is(y, "RasterLayer")) {
    stop("y, is not RasterLayer")
  }

  if (!identical(raster::extent(x), raster::extent(y))) {
    stop("x and y do not have an identical extent")
  }
  r1 <- x[]
  r2 <- y[]
  r1 <- r1 / sum(r1)
  r2 <- r2 / sum(r2)
  ## bhattacharyya's afinity
  sum(sqrt(r1 * r2))
}

#' @export
hr_ba.list <- function(x, ...) {

  # check all elements are hr
  if(!all(sapply(x, inherits, "hr_prob"))) {
    stop("Not all x are probabilistic home-range estimates.")
  }

  if (length(x) < 2) {
    stop("At least two home range estimates are needed")
  }

  nn <- if (is.null(names(x))) 1:length(x) else names(x)

  if (any(nchar(nn) == 0)) {
    stop("Names must be different from ''")
  }

  if (any(duplicated(nn))) {
    stop("Duplicated names are not permitted")
  }

  uds <- lapply(x, hr_ud)
  res <- expand.grid(from = nn, to = nn)

  tidyr::expand_grid(from = nn, to = nn) %>%
    dplyr::filter(from != to) %>%
    dplyr::mutate(ba = purrr::map2_dbl(from, to, function(i, j) {
      hr_ba(uds[[i]], uds[[j]])
    }))
}

