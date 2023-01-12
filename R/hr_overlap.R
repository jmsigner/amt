#' Methods to calculate home-range overlaps
#'
#' Methods to calculate the overlap of two or more home-range estimates.
#'
#' @param x,y A home-range estimate
#' @param type `[character](1)` \cr Type of index, should be one of `hr`, `phr`,
#'   `vi`, `ba`, `udoi`, or `hd`.
#' @param conditional `[logical](1)` \cr Whether or not conditional UDs are
#'   used. If `TRUE` levels from that were used to estimate home ranges will be
#'   used.
#' @template dots_none
#' @return \code{data.frame} with the isopleth level and area in units of the
#'   coordinate reference system.
#' @name hr_overlaps
#' @export
hr_overlap <- function (x, ...) {
  UseMethod("hr_overlap", x )
}

#' @export
#' @rdname hr_overlaps
hr_overlap.hr <- function(x, y, type = "hr", conditional = FALSE, ...) {

  checkmate::assert_class(x, "hr")
  checkmate::assert_class(y, "hr")
  checkmate::assert_character(type, len = 1)
  checkmate::assert_logical(conditional, len = 1)

  if (!type %in% c("hr", "phr", "vi", "ba", "udoi", "hd")) {
    stop("type should be one of: hr, phr, vi, ba, udoi, or hd")
  }

  if (type == "hr") {
    x <- hr_isopleths(x)
    y <- hr_isopleths(y)
    hr_base(x, y)
  } else {
    # both of correct class
    if (is(x, "hr_prob") & is(y, "hr_prob")) {
      xud <- hr_ud(x)
      yud <- hr_ud(y)

      # Check that both are of the same raster extent
      if (identical(terra::ext(xud), terra::ext(yud)) &
          all(terra::res(xud) == terra::res(yud))) {

        # Do I have to get conditional uds?
        if (conditional) {
          # Check x and y have the same levels
          if (!all(x$levels == y$levels)) {
            stop("x and y should have the same level for conditional hr_overlap")
          }
          tibble(
            levels = x$levels,
            overlap = purrr::map_dbl(levels, ~ vol_base(
              get_cond_ud(x, level = .x)[],
              get_cond_ud(y, level = .x)[],
              type = type
            ))
          )
        } else {
          tibble(
            levels = 1,
            overlap = vol_base(xud[], yud[], type)
          )
        }
      } else {
        stop("Extend and resolution differ, make sure to use a template raster.")
      }
    } else {
      stop("Both x and y should be probabilistic estimators.")
    }
  }
}

get_cond_ud <- function(x, level = 1) {
    cud <- hr_cud(x) # This is the cumulative UD (i.e., something like the CDF)
    ud <- hr_ud(x)
    ud[cud > level] <- 0
    ud[] <- ud[] / level
    ud
}

hr_base <- function(x, y) {
  if(all(x$levels == y$levels)) {
    if (length(x$level) == 1) {
      return(
        tibble::tibble(
          level = x$level,
          overlap = hr_base_ov(x, y))
      )
    } else if (length(x$level) > 1) {
      return(
        tibble::tibble(
          levels = x$level,
          overlap = sapply(1:nrow(x), function(i) hr_base_ov(x[i, ], y[i, ])))
      )
    }
  } else {
    stop("Not all levels of x in y.")
  }
}

vol_base <- function(ud_i, ud_j, type) {
  if (type == "phr") {
    sum(ud_j[ud_i > 0])
  } else if (type == "vi") {
    sum(pmin(ud_i, ud_j))
  } else if (type == "ba") {
    sum(sqrt(ud_i) * sqrt(ud_j))
  } else if (type == "udoi") {
    A_ij <- sum(ud_i > 0 & ud_j > 0)
    A_ij * sum(ud_i * ud_j)
  } else if (type == "hd") {
    2 * (1 - sum(sqrt(ud_i) * sqrt(ud_j)))
  }
}


#' @rdname hr_overlaps
#' @export
#' @param  which `[character = "consecutive"]` \cr Should only consecutive overlaps be calculated or all combinations?
#' @param  labels `[character=NULL]` \cr Labels for different instances. If `NULL` (the default) numbers will be used.
hr_overlap.list <- function(
  x, type = "hr", conditional = FALSE,  which = "consecutive", labels = NULL, ...) {

  # check all elements are hr
  if(!all(sapply(x, inherits, "hr"))) {
    stop("Not all x are home-range estimates.")
  }

  if (length(x) < 2) {
    stop("At least two home range estimates are needed")
  }

  checkmate::assert_character(which)
  if (!which %in% c("all", "consecutive", "one_to_all")) {
    stop("Which should be one of 'all', 'consecutive' or 'one_to_all'")
  }

  nn <- if (is.null(names(x))) 1:length(x) else names(x)

  if (!is.null(labels)) {
    if (!is.character(labels)) {
      labels <- as.character(labels)
      message("labels coerced to character.")
    }
    if (length(labels) == length(nn)) {
      nn <- labels
      names(x) <- labels
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

  levels <- lapply(x, function(y) y$levels)
  if (!all(sapply(levels[-1], function(x) all(levels[[1]] == x)))) {
    stop("Not all levels of first home range est are in the others as well.")
  }

  grid <- if (which == "consecutive") {
    tibble(from = nn[-length(nn)], to = nn[-1])
  } else if (which == "all") {
    tidyr::expand_grid(from = nn, to = nn) |>
      dplyr::filter(from != to)
  } else if (which == "one_to_all") {
    tibble(from = nn[1], to = nn[-1])
  }


  grid |>
    dplyr::mutate(overlap = purrr::map2(from, to, function(i, j) {
      hr_overlap(x[[i]], x[[j]], type = type, conditional = conditional)
    })) |>
    tidyr::unnest(cols = "overlap")
}


#' Calculate the overlap between a home-range estimate and a polygon
#'
#' Sometimes the percentage overlap between a spatial polygon an a home-range is required.
#'
#' @param x A home-range estimate.
#' @param sf An object of class `sf` containing polygons
#' @param direction The direction.
#' @param feature_names  optional feature names
#' @return A tibble
#' @export

hr_overlap_feature <- function(x, sf, direction = "hr_with_feature", feature_names = NULL) {

  checkmate::assert_class(x, "hr")
  checkmate::assert_character(direction, len = 1)

  if (!(is(sf, "sf") | is(sf, "sfc"))) {
    stop("sf needs be of class `sf` or `sfc`.")
  }

  if (is(sf, "sfc")) {
    sf <- sf::st_as_sf(sf)
  }

  if (!sf::st_geometry_type(sf) %in% c("POLYGON", "MULTIPOLYGON")) {
    stop("`sf` should be a (MULTI)POLYGON.")
  }

  if (!direction %in% c("hr_with_feature", "feature_with_hr")) {
    stop("Wrong direction")
  }
  if (!is.null(feature_names)) {
    if (length(feature_names) != nrow(sf)) {
      stop("`feature_names` do not match the number of features in `sf`.")
    }
  }
  names_hr <- x$levels
  names_sf <- if(is.null(feature_names)) 1:nrow(sf) else feature_names

  if (direction == "hr_with_feature") {
    hr_feature(hr_isopleths(x), sf) |>
      mutate(from = names_hr[from], to = names_sf[to])
  } else if (direction == "feature_with_hr") {
    hr_feature(sf, hr_isopleths(x)) |>
      mutate(from = names_sf[from], to = names_hr[to])
  }
}


hr_feature <- function(x, y) {
  if (nrow(x) == 1 & nrow(y) == 1) {
    return(tibble::tibble(from = 1, to = 1,
                          overlap = hr_base_ov(x, y)))
  } else {
    res <- expand.grid(from = 1:nrow(x), to = 1:nrow(y))
    return(
      tibble::tibble(
        from = res$from,
        to = res$to,
        overlap = purrr::map_dbl(1:nrow(res), function(i) hr_base_ov(x[res$from[i], ], y[res$to[i], ]))
      )
    )
  }
}

hr_base_ov <- function(x, y) {
  if (sf::st_intersects(x, y, sparse = FALSE)) {
    suppressWarnings(ol <- sf::st_intersection(x, y))
    as.numeric(sf::st_area(ol) / sf::st_area(x))
  } else {
    0
  }
}
