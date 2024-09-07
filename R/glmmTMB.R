# These functions provide support for glmmTMB models

# **Important Note**
# The method described by Muff et al. (2018) of fitting using glmmTMB
# (i.e., glmmTMB(doFit = FALSE), etc.) causes issues with predict()
# with newdata. See here: https://github.com/glmmTMB/glmmTMB/issues/535
# The result of predict() ends up as a vector of 0s.
# Not sure why, but it is important to make users aware that they need to
# check their fitting procedure. Correct method (to be) shown in vignette.

#' Check for `glmmTMB`
#'
#' Internal function to stop if `glmmTMB` is not installed
check_glmmTMB <- function() {
  if (!rlang::is_installed("glmmTMB")) {
    stop("You must install package 'glmmTMB' for this function to work.")
  }
}

#' Return names of random effects in glmmTMB model
#'
#' Internal function to return random effects and identify strata variable
RE_glmmTMB <- function(object) {
  # Names of random effects
  REs <- object$modelInfo$grpVar

  # The one with the most levels should define the strata
  RE_levs <- apply(object$frame[, REs, drop = FALSE], 2, function(x) length(unique(x)))

  # Stratum variable
  strat_var <- names(RE_levs)[which(RE_levs == max(RE_levs))]

  # Other REs
  other_RE <- REs[which(REs != strat_var)]

  # Combine in list
  res <- list(stratum = strat_var,
              other = other_RE)
  return(res)
}
