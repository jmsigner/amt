% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/movement_metrics.R
\name{movement_metrics}
\alias{movement_metrics}
\alias{straightness}
\alias{cum_dist}
\alias{tot_dist}
\alias{msd}
\alias{intensity_use}
\alias{sinuosity}
\alias{tac}
\title{Movement metrics}
\usage{
straightness(x, ...)

cum_dist(x, ...)

tot_dist(x, ...)

msd(x, ...)

intensity_use(x, ...)

sinuosity(x, ...)

tac(x, ...)
}
\arguments{
\item{x}{\code{[track_xy, track_xyt]} \cr A track created with \code{make_track}.}

\item{...}{Further arguments, none implemented.}
}
\value{
A numeric vector of length one.
}
\description{
Functions to calculate metrics such as straightness, mean squared displacement (msd), intensity use,
sinuosity, mean turn angle correlation (\code{tac}) of a track.
}
\details{
The intensity use is calculated by dividing the total movement distance (\code{tot_dist}) by the square of the area of movement (= minimum convex polygon 100).
}
\examples{
data(deer)

tot_dist(deer)
cum_dist(deer)
straightness(deer)
msd(deer)
intensity_use(deer)

}
\references{
\itemize{
\item \insertRef{abrahams2017}{amt}
\item \insertRef{almeida2010}{amt}
\item \insertRef{swihart1985}{amt}
}
}
