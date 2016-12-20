#' @export
hr_area <- function(x, ...) {
  UseMethod("hr_area", x)
}

#' @export
hr_area.amt_mcp <- function(x, ...) {
  as_data_frame(x$mcp)
}
