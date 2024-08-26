#' Selects relocations that fit a new time series
 #'
 #' Functions to only selects relocations that can be aligned with a new time series (within some tolerance).
 #' @param x A track.
 #' @param new.times The new time trajectory.
 #' @param tolerance The tolerance between observed time stamps and new time stamps in seceonds. This should be either a vector of length 1 or length `new.times`.
 #' @template dots_none
 #' @return A `track_xyt`.
 #' @name track_align
 #' @export

 track_align <- function(x, ...) {
   UseMethod("track_align", x)
 }

 #' @rdname track_align
 #' @export
 track_align.track_xyt <- function(x, new.times, tolerance, ...) {

   # checks
   if (max(new.times) <= min(x$t_)) {
     stop("new time stamps do not overlap with observed time stamps.")
   }

   checkmate::assert_numeric(tolerance, lower = 0)

   if (!length(tolerance) %in% c(1, length(new.times))) {
     stop("Tolerance should be either 1 or `length(new.times)`.")
   }

   # Do the calculations
   obs.times <- x$t_

   # Calculate temporal differences between observed and new time stamps
   r1 <- outer(as.numeric(obs.times), as.numeric(new.times),
               FUN = function(x, y) abs(x - y))

   ids <- apply(r1, 2, which.min)
   ids.ok <- abs(as.numeric(obs.times[ids]) - as.numeric(new.times)) <= tolerance

   x <- x[ids, c("x_", "y_")]
   x$t_ <- new.times
   x$burst_ = ids.ok

   # Remove observations that are off in time
   rx <- rle(x$burst_)

   # If all false
   if (all(!rx$values)) {
     stop("No observed relocations within tolerance of new time stamps.")
   }

   lt <- rx$lengths[rx$values]
   x <- x[x$burst_, ]
   x$burst_ <- rep(1:length(lt), lt)

   if (all(x$burst_)) {
     x[["burst_"]] <- NULL
     # make sure track is not bursted
   } else {
     # make sure track is bursted
   }

   x

 }
