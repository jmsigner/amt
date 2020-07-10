#' Time of the day when a fix was taken
#'
#' A convenience wrapper around `maptools::sunriset` and `maptools::crepuscule` to extract if a fix was taken during day or night (optionally also include dawn and dusk).
#'
#' @param x `[track_xyt,steps_xyt]`\cr A track or steps.
#' @param solar.dep `[numeric(1,n)=6]`\cr The angle of the sun below the horizon in degrees. Passed to `maptools::crepuscule`.
#' @param include.crepuscule `[logical(1)=TRUE]`\cr Should dawn and dusk be included.
#' @param where `[character(1)="end"]{"start", "end", "both"}` For `steps`, should the start, end or both time points be used?
#' @template dots_none
#' @name time_of_day
#' @export
#' @examples
#' data(deer)
#' deer %>% time_of_day()
#' deer %>% steps_by_burst %>% time_of_day()
#' deer %>% steps_by_burst %>% time_of_day(where = "start")
#' deer %>% steps_by_burst %>% time_of_day(where = "end")
#' deer %>% steps_by_burst %>% time_of_day(where = "both")
#'


time_of_day <- function(x, ...) {
  UseMethod("time_of_day", x)
}

#' @export
#' @rdname time_of_day
time_of_day.track_xyt <- function(x, solar.dep = 6, include.crepuscule = FALSE, ...) {
  x["tod_"] <- time_of_day_base(x, x[["t_"]], solar.dep = solar.dep, include.crepuscule = include.crepuscule, ...)
  x
}

#' @export
#' @rdname time_of_day
time_of_day.steps_xyt <- function(x, solar.dep = 6, include.crepuscule = FALSE, where = "end", ...) {
    if (where == "both") {
      x["tod_start_"] <- time_of_day_base(x, x[["t1_"]], solar.dep = solar.dep, include.crepuscule = include.crepuscule, end = FALSE, ...)
      x["tod_end_"] <- time_of_day_base(x, x[["t2_"]], solar.dep = solar.dep, include.crepuscule = include.crepuscule, end = TRUE, ...)

    } else {
      if (where == "end") {
        x["tod_end_"] <- time_of_day_base(x, x[["t2_"]], solar.dep = solar.dep, include.crepuscule = include.crepuscule, end = TRUE, ...)
      } else if (where == "start") {
        x["tod_start_"] <- time_of_day_base(x, x[["t1_"]], solar.dep = solar.dep, include.crepuscule = include.crepuscule, end = FALSE, ...)
      }
    }
    x
}


time_of_day_base <- function(x, t, solar.dep, include.crepuscule, end = TRUE) {

  # Remove NA coordinates, so we can propagate NA's
  idx <- if (is(x, "track_xy")) {
    is.na(x[["x_"]]) | is.na(x[["y_"]]) | is.na(t)
  } else if (is(x, "steps_xy")) {
    if (end) {
      is.na(x[["x2_"]]) | is.na(x[["y2_"]]) | is.na(t)
    } else {
      is.na(x[["x1_"]]) | is.na(x[["y1_"]]) | is.na(t)
    }
  }
  res <- rep(NA, length(idx))


  if (any(!idx)) {

    x <- x[!idx, ]
    t <- t[!idx]

    if (suppressWarnings(has_crs(x))) {
      pts <- sp::spTransform(as_sp(x, end = end), sp::CRS("+init=epsg:4326"))
    } else {
      stop("No CRS found.")
    }

    sunr <- as.numeric(maptools::sunriset(pts, t, direction = "sunrise",
                                          POSIXct.out = TRUE)$time)
    suns <- as.numeric(maptools::sunriset(pts, t, direction = "sunset",
                                          POSIXct.out = TRUE)$time)
    tt <- as.numeric(t)
    if (include.crepuscule) {
      dawn <- as.numeric(maptools::crepuscule(
        pts, t, direction = "dawn",
        solarDep = solar.dep, POSIXct.out = TRUE)$time)
      dusk <- as.numeric(maptools::crepuscule(
        pts, t, direction = "dusk", solarDep = solar.dep,
        POSIXct.out = TRUE)$time)
      if (anyNA(c(dawn, dusk))) {
        stop("dawn or dusk can not contain NA's, try adjusting the `solar.dep`")
      }
      names <- c("night", "dawn", "day", "dusk", "night")
      xx <- names[apply(cbind(tt, dawn, sunr, suns, dusk), 1,
                        function(x) if(any(is.na(x))) NA else base::findInterval(x[1], x[2:5])) + 1]
    } else {
      names <- c("night", "day", "night")
      xx <- names[apply(cbind(tt, sunr, suns), 1,
                        function(x) if(any(is.na(x))) NA else base::findInterval(x[1], x[2:3])) + 1]
    }

    if (any(is.na(xx))) {
      message("For some points NA was returned for sunrise/sunset. Maybe high latitude during summer/winter?")
    }
    res[!idx] <- xx
  }


  if (include.crepuscule) {
    factor(res, levels = c("day", "dusk", "night", "dawn"))
  } else {
    factor(res, levels = c("day", "night"))
  }

}
