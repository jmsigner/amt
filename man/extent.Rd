% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extent.R
\name{extent}
\alias{extent}
\alias{extent_x}
\alias{extent_y}
\alias{extent_both}
\alias{extent_max}
\title{Extent of a track}
\usage{
extent_x(x, ...)

extent_y(x, ...)

extent_both(x, ...)

extent_max(x, ...)
}
\arguments{
\item{x}{\code{[track_xy, track_xyt, steps]} \cr Either a track created with \code{mk_track} or \code{track}, or \code{steps}.}

\item{...}{Further arguments, none implemented.}
}
\value{
Numeric vector with the extent.
}
\description{
Obtain the extent of a track in \code{x} \code{y} or both directions
}
