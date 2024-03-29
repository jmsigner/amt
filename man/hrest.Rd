% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hr_akde.R, R/hr_documentation.R, R/hr_kde.R,
%   R/hr_locoh.R, R/hr_mcp.R, R/hr_od.R
\name{hr_akde}
\alias{hr_akde}
\alias{hr_akde.track_xyt}
\alias{hrest}
\alias{hr_kde}
\alias{hr_kde.track_xy}
\alias{hr_locoh}
\alias{hr_locoh.track_xy}
\alias{hr_mcp}
\alias{hr_od}
\title{Home ranges}
\usage{
hr_akde(x, ...)

\method{hr_akde}{track_xyt}(
  x,
  model = fit_ctmm(x, "iid"),
  keep.data = TRUE,
  trast = make_trast(x),
  levels = 0.95,
  wrap = FALSE,
  ...
)

hr_kde(x, ...)

\method{hr_kde}{track_xy}(
  x,
  h = hr_kde_ref(x),
  trast = make_trast(x),
  levels = 0.95,
  keep.data = TRUE,
  wrap = FALSE,
  ...
)

hr_locoh(x, ...)

\method{hr_locoh}{track_xy}(
  x,
  n = 10,
  type = "k",
  levels = 0.95,
  keep.data = TRUE,
  rand_buffer = 1e-05,
  ...
)

hr_mcp(x, ...)

hr_od(x, ...)
}
\arguments{
\item{x}{\code{[track_xy, track_xyt]} \cr A track created with \code{make_track}.}

\item{...}{Further arguments, none implemented.}

\item{model}{A continuous time movement model. This can be fitted either with \code{ctmm::ctmm.fit} or \code{fit_ctmm}.}

\item{keep.data}{\verb{[logic(1)]} \cr Should the original tracking data be included in the estimate?}

\item{trast}{\verb{[SpatRast]} \cr A template raster for kernel density home-ranges.}

\item{levels}{\verb{[numeric]} \cr The isopleth levels used for calculating home
ranges. Should be \verb{0 < level < 1}.}

\item{wrap}{\verb{[logical(1)]} \cr If \code{TRUE} the UD is wrapped (see \code{terra::wrap()}).}

\item{h}{\verb{[numeric(2)]} \cr The bandwidth for kernel density estimation.}

\item{n}{\verb{[integer(1)]} \cr The number of neighbors used when calculating
local convex hulls.}

\item{type}{\code{k}, \code{r} or \code{a}. Type of LoCoH.}

\item{rand_buffer}{\verb{[numeric(1)]} \cr Random buffer to avoid polygons with
area 0 (if coordinates are numerically identical).}
}
\value{
A \code{hr}-estimate.
}
\description{
Functions to calculate animal home ranges from a \verb{track_xy*}. \code{hr_mcp}, \code{hr_kde}, and \code{hr_locoh} calculate the minimum convex
polygon, kernel density, and local convex hull home range respectively.
}
\examples{
\donttest{
data(deer)
mini_deer <- deer[1:100, ]

# MCP ---------------------------------------------------------------------
mcp1 <- hr_mcp(mini_deer)
hr_area(mcp1)

# calculated MCP at different levels
mcp1 <- hr_mcp(mini_deer, levels = seq(0.3, 1, 0.1))
hr_area(mcp1)

# CRS are inherited
get_crs(mini_deer)
mcps <- hr_mcp(mini_deer, levels = c(0.5, 0.95, 1))
has_crs(mcps)

# Kernel density estimaiton (KDE) -----------------------------------------
kde1 <- hr_kde(mini_deer)
hr_area(kde1)
get_crs(kde1)

# akde
data(deer)
mini_deer <- deer[1:20, ]
ud1 <- hr_akde(mini_deer) # uses an iid ctmm
ud2 <- hr_akde(mini_deer, model = fit_ctmm(deer, "ou")) # uses an OU ctmm
}
# od
\donttest{
data(deer)
ud1 <- hr_od(deer) # uses an iid ctmm
ud2 <- hr_akde(deer, model = fit_ctmm(deer, "ou")) # uses an OU ctmm
}
}
\references{
Worton, B. J. (1989). Kernel methods for estimating the utilization distribution in home-range studies. \emph{Ecology, 70(1)}, 164-168.
C. H. Fleming, W. F. Fagan, T. Mueller, K. A. Olson, P. Leimgruber, J. M. Calabrese, “Rigorous home-range estimation with movement data: A new autocorrelated kernel-density estimator”, Ecology, 96:5, 1182-1188 (2015).

Fleming, C. H., Fagan, W. F., Mueller, T., Olson, K. A., Leimgruber, P., & Calabrese, J. M. (2016). Estimating where and how animals travel: an optimal framework for path reconstruction from autocorrelated tracking data. Ecology, 97(3), 576-582.
}
