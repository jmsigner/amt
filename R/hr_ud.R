hr_ud <- function(x, ...) {
  UseMethod("hr_ud", x)
}

hr_ud.kde <- function(x, ...) {
  x$ud
}
