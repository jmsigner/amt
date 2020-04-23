#' Convert
#'
#' @param x A `tibble` with a `list column` with individual home ranges.
#' @param col The column where the home
#' @param ... Additional columns that should be transferred to the new `tible`.
#'
#' @return A `data.frame` with a simple feature column (from the `sf`) package.
#' @export
#' @name hr_to_sf
#'
#' @examples
#'
#' data("amt_fisher")
#' hr <- amt_fisher %>% nest(data = -id) %>%
#'   mutate(hr = map(data, hr_mcp), n = map_int(data, nrow)) %>%
#'   hr_to_sf(hr, id, n)
#'
#' hr <- amt_fisher %>% nest(data = -id) %>%
#'   mutate(hr = map(data, hr_kde), n = map_int(data, nrow)) %>%
#'   hr_to_sf(hr, id, n)
#'
#'
#' \dontrun{
#' ggplot(hr) + geom_sf()
#'
#' }
#'

hr_to_sf <- function(x, ...) {
  UseMethod("hr_to_sf", x)
}

#' @export
#' @rdname hr_to_sf
hr_to_sf.tbl_df <- function(x, col, ...) {

  col <- rlang::enquo(col)
  cols <- rlang::enquos(...)

  x1 <- dplyr::select(x, !!col)
  if (!all(sapply(x1[[1]], is, "hr"))) {
    stop("col can contain only home ranges")
  }

  x1 <- lapply(x1[[1]], hr_isopleths)

  x2 <- dplyr::select(x, !!!cols)
  x2 <- x2[rep(1:nrow(x), sapply(x1, nrow)), ]

  x1 <- do.call("rbind", lapply(x1, sf::st_as_sf))
  dplyr::bind_cols(x1, x2)
}

#' @export
hr_to_sf.list <- function(x, ...) {

  if (!all(sapply(x, is, "hr"))) {
    stop("Not all elements are hr estimates")
  }

  nn <- if (is.null(names(x))) 1:length(x) else names(x)

  if (any(nchar(nn) == 0)) {
    stop("Names must be different from ''")
  }

  if (any(duplicated(nn))) {
    stop("Duplicated names are not permitted")
  }

  x <- lapply(x, hr_isopleths)
  names <- rep(nn, sapply(x, nrow))

  xx <- do.call("rbind", x)
  sf::st_as_sf(dplyr::bind_cols(name = names, xx))

}
