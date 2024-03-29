% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distributions.R
\name{update_sl_distr}
\alias{update_sl_distr}
\alias{update_ta_distr}
\title{Update movement distributions}
\usage{
update_sl_distr(
  object,
  beta_sl = "sl_",
  beta_log_sl = "log_sl_",
  beta_sl_sq = "sl_sq_",
  beta_log_sl_sq = "log_sl_sq_",
  ...
)

update_ta_distr(object, beta_cos_ta = "cos_ta_", ...)
}
\arguments{
\item{object}{\verb{[fit_clogit]} \cr A fitted iSSF model.}

\item{beta_sl}{\verb{[character]} \cr The name of the coefficient of the step length.}

\item{beta_log_sl}{\verb{[character]} \cr The name of the coefficient of the log of the step length.}

\item{beta_sl_sq}{\verb{[character]} \cr The name of the coefficient of the square of the step length.}

\item{beta_log_sl_sq}{\verb{[character]} \cr The name of the coefficient of the square of log of the step length.}

\item{...}{Further arguments, none implemented.}

\item{beta_cos_ta}{\verb{[character]} \cr The name of the coefficient of cosine of the turning angle.}
}
\value{
An \code{amt_distr} object, which consists of a list with the \code{name} of
the distribution and its parameters (saved in \code{params}).
}
\description{
Update tentative step length or turning angle distribution from a fitted iSSF.
}
\examples{

# Fit an SSF, then update movement parameters.
data(deer)
mini_deer <- deer[1:100, ]
sh_forest <- get_sh_forest()

# Prepare data for SSF
ssf_data <- mini_deer |>
  steps_by_burst() |>
  random_steps(n = 15) |>
  extract_covariates(sh_forest) |>
  mutate(forest = factor(forest, levels = 1:0,
                    labels = c("forest", "non-forest")),
  cos_ta_ = cos(ta_),
  log_sl_ = log(sl_))

# Check tentative distributions
# Step length
sl_distr_params(ssf_data)
attr(ssf_data, "sl_")
#    Turning angle
ta_distr_params(ssf_data)

# Fit an iSSF
m1 <- ssf_data |>
  fit_issf(case_ ~ forest +
               sl_ + log_sl_ + cos_ta_ +
               strata(step_id_))

# Update step length distribution
new_gamma <- update_sl_distr(m1)

# Update turning angle distribution
new_vm <- update_ta_distr(m1)

# It is also possible to use different step length distributions

# exponential step-length distribution
s2 <- mini_deer |> steps_by_burst()
s2 <- random_steps(s2, sl_distr = fit_distr(s2$sl_, "exp"))
m2 <- s2 |>
  fit_clogit(case_ ~ sl_ + strata(step_id_))
update_sl_distr(m2)

# half normal step-length distribution
s3 <- mini_deer |> steps_by_burst()
s3 <- random_steps(s3, sl_distr = fit_distr(s3$sl_, "hnorm"))
m3 <- s3 |>
  mutate(sl_sq_ = sl_^2) |>
  fit_clogit(case_ ~ sl_sq_ + strata(step_id_))
update_sl_distr(m3)

# log normal step-length distribution
s4 <- mini_deer |> steps_by_burst()
s4 <- random_steps(s4, sl_distr = fit_distr(s4$sl_, "lnorm"))
m4 <- s4 |>
  mutate(log_sl_ = log(sl_), log_sl_sq_ = log(sl_)^2) |>
  fit_clogit(case_ ~ log_sl_ + log_sl_sq_ + strata(step_id_))
update_sl_distr(m4)



}
\references{
\insertRef{fieberg2020guide}{amt}
}
\seealso{
Wrapper to fit a distribution to data \code{\link{fit_distr}()}
}
\author{
Brian J. Smith and Johannes Signer
}
