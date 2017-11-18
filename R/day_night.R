#' Extract if a fix was taken during day or night (optionally also include dawn and dusk).
#'
#' A convenience wrapper around `maptools::sunriset` and `maptools::repuscule`.
#'
#' @param .tbl `[track_xyt,steps_xyt]`\cr A track or steps.
#' @param x `[symbol(1)=t_]`\cr Name of the time column, usually this is `t_`.
#' @param solar.dep `[numeric(1,n)=6]`\cr The angle of the sun below the horizon in degrees. Passed to `maptools::crepuscule`.
#' @param include.crepuscule `[logical(1)=TRUE]`\cr Should dawn and dusk be included.
#' @template dots_none
#' @name day_night
#' @export
#' @examples
#' data(deer)
#' deer %>% mutate(time_of_day = day_night(t_))
#' deer %>% steps_by_burst %>%
#'   mutate(time_of_day = day_night(t_))
#'
#' @export
day_night <- function(.tbl, x = t_, solar.dep = 6, include.crepuscule = TRUE, ...) {
  if (suppressWarnings(has_crs(.tbl))) {
    pts <- sp::spTransform(as_sp(.tbl, end = TRUE), sp::CRS("+init=epsg:4326"))
  } else {
    stop("No CRS found.")
  }
  day_night_base(pts, .tbl[[deparse(substitute(x))]], solar.dep = solar.dep, include.crepuscule = include.crepuscule, ...)
}


#' @noRd
day_night_base <- function(pts, x, solar.dep, include.crepuscule, ...) {

  sunr <- as.numeric(maptools::sunriset(pts, x, direction = "sunrise", POSIXct.out = TRUE)$time)
  suns <- as.numeric(maptools::sunriset(pts, x, direction = "sunset", POSIXct.out = TRUE)$time)

  tt <- as.numeric(x)

  if (include.crepuscule) {
    dawn <- as.numeric(maptools::crepuscule(pts, x, direction = "dawn", solarDep = solar.dep, POSIXct.out = TRUE)$time)
    dusk <- as.numeric(maptools::crepuscule(pts, x, direction = "dusk", solarDep = solar.dep, POSIXct.out = TRUE)$time)
    if (anyNA(c(dawn, dusk))) {
      stop("dawn or dusk can not contain NA's, try adjusting the `solar.dep`")
    }
    names <- c("night", "dawn", "day", "dusk", "night")
    names[apply(cbind(tt, dawn, sunr, suns, dusk), 1, function(x) base::findInterval(x[1], x[2:5])) + 1]
  } else {
    names <- c("night", "day", "night")
    names[apply(cbind(tt, sunr, suns), 1, function(x) base::findInterval(x[1], x[2:3])) + 1]
  }
}


