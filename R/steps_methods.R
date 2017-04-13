#' @export
is_step <- function(x) {
  if (all(c("x1_", "y1_", "x2_", "y2_", "ta_", "sl_") %in% names(x))) {
    TRUE
  } else {
    FALSE
  }
}

#' @export
check_step <- function(x) {
  if (is_step(x)) {
    return(TRUE)
  } else {
    stop("x is not an object of class step")
  }
}
