hr_intersects_many <- function(..., method = "intersection",
                          how = c("all_pairs", "consec", "all")) {

  x <- list(...)

  if (is.list(x[[1]])) {
    x <- x[[1]]
  }

  if (length(x) < 2) {
    stop("At least two home ranges are needed")
  }

  if (!all(purrr::map_lgl(x, is, "hr"))) {
    stop("Not all elemets are home ranges")
  }

}

hr_intersection <- function(x, y, method = "intersection", level = 0.9,
                          percent = TRUE) {

 x <- hr_isopleths(x, levels = level)
 y <- hr_isopleths(y, levels = level)

 # Intersections
 int <- sf::st_intersection(sf::st_geometry(x), y)

 if (percent) {
   as.vector(sf::st_area(int) / sf::st_area(x))
 } else {
   int
 }
}
