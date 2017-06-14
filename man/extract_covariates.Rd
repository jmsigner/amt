% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_covariates.R
\name{extract_covariates}
\alias{extract_covariates}
\alias{extract_covariates.track_xy}
\alias{extract_covariates.random_points}
\alias{extract_covariates.steps}
\title{Extract covariate values.}
\usage{
extract_covariates(x, ...)

\method{extract_covariates}{track_xy}(x, covariates, ...)

\method{extract_covariates}{random_points}(x, covariates, ...)

\method{extract_covariates}{steps}(x, covariates, where = "end", ...)
}
\arguments{
\item{x}{A track or step.}

\item{...}{Further arguments, none implemented.}

\item{covariates}{A \code{RasterLayeR} or a \code{RasterStack}.}

\item{where}{A character scalar, for \code{steps} this can take the values \code{start} or \code{end}.}
}
\description{
Functions to extract the value of covariates for tracks or steps.
}