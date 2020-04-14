#' Convert
#'
#' @param x A `tibble` with a `list column` with individual home ranges.
#' @param col The column where the home
#' @param ... Additional columns that should be transfered to the new `tible`.
#'
#' @return A `data.frame` with a simple feature column (from the `sf`) package.
#' @export
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

hr_to_sf <- function(x, col, ..., level = 0.95) {

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
