#' Calculate log-RSS for a fitted model
#'
#' Calculate log-RSS(x1, x2) for a fitted RSF or (i)SSF
#'
#' @param object `[a fitted model]` \cr A fitted RSF or (i)SSF model. For possible
#' models, see \code{\link{supported_models}()}
#' @param x1 `[data.frame]` \cr A `data.frame` representing the habitat values
#' at location x_1. Must contain all fitted covariates as expected by
#' `predict()`.
#' @param x2 `[data.frame]` \cr A 1-row `data.frame` representing the single
#' hypothetical location of x_2. Must contain all fitted covariates as expected
#' by `predict()`.
#' @param ci `[character]` \cr Method for estimating confidence intervals around
#' log-RSS. `NA` skips calculating CIs. Character string `"se"` uses standard error
#' method and `"boot"` uses empirical bootstrap method.
#' @param ci_level `[numeric]` \cr Level for confidence interval. Defaults to 0.95
#' for a 95% confidence interval.
#' @param n_boot `[integer]` \cr Number of bootstrap samples to estimate confidence
#' intervals. Ignored if `ci != "boot"`.
#' @template dots_none
#'
#' @details This function assumes that the user would like to compare relative
#' selection strengths from at least one proposed location (`x1`) to exactly
#' one reference location (`x2`).
#'
#' The objects `object$model`, `x1`, and `x2` will be passed to
#' `predict()`. Therefore, the columns of `x1` and `x2` must match
#' the terms in the model formula exactly.
#'
#' `glmmTMB` objects
#'
#' `log_rss()` now supports models fit with `glmmTMB`, particularly of interest
#' for those using the method of Muff et al. (2020) to fit a mixed effects
#' model. \emph{Note} that at this point, predictions are only at the
#' population level and do not account for individual variation. Currently,
#' only `ci = "se"` is supported.
#'
#' `gam` objects
#'
#' `log_rss()` now supports models fit with `mgcv::gam()`. This method can
#' be used to fit models with splines and/or random effects. See Klappstein
#' et al. (2024) for details of model structure. Unlike with models fit via
#' `glmmTMB`, predictions from models fit with `mgcv::gam()` do support
#' prediction at the individual level (i.e., considering the random effects).
#' Currently, only `ci = "se"` is supported.
#'
#' @author Brian J. Smith
#'
#' @return Returns a `list` of class `log_rss` with four entries:
#' - `df`: A `data.frame` with the covariates and the `log_rss`
#' - `x1`: A `data.frame` with covariate values for `x1`.
#' - `x2`: A `data.frame` with covariate values for `x2`.
#' - `formula`: The formula used to fit the model.
#'
#' @seealso See Avgar \emph{et al.} 2017 for details about relative
#' selection strength.
#'
#' Default plotting method available: \code{\link{plot.log_rss}()}
#'
#' For details on models currently supported in `amt`:
#' \code{\link{supported_models}()}
#'
#' @references
#' - Avgar, T., Lele, S.R., Keim, J.L., and Boyce, M.S.. (2017). Relative Selection
#' Strength: Quantifying effect size in habitat- and step-selection inference.
#' *Ecology and Evolution*, 7, 5322–5330. doi: 10.1002/ece3.3122
#' - Fieberg, J., Signer, J., Smith, B., & Avgar, T. (2021). A "How to" guide
#' for interpreting parameters in habitat-selection analyses.
#' *Journal of Animal Ecology*, 90(5), 1027-1043. doi: 10.1111/1365-2656.13441
#' - Muff, S., Signer, J. & Fieberg, J.R. (2020). Accounting for individual-
#' specific variation in habitat-selection studies: Efficient estimation of
#' mixed-effects models using Bayesian or frequentist computation.
#' *Journal of Animal Ecology*, 89, 80–92. doi: 10.1111/1365-2656.13087
#' - Klappstein, N.J., Michelot, T., Fieberg, J., Pedersen, E.J. &
#' Mills-Flemming, J. (2024). Step selection functions with non‐linear and
#' random effects. *Methods in Ecology and Evolution*, 15(8), 1332-1346.
#' doi: 10.1111/2041-210X.14367
#'
#' @examples
#'
#' \donttest{
#' # RSF -------------------------------------------------------
#' # Fit an RSF, then calculate log-RSS to visualize results.
#'
#' # Load packages
#' library(ggplot2)
#'
#' # Load data
#' data("amt_fisher")
#' amt_fisher_covar <- get_amt_fisher_covars()
#'
#' # Prepare data for RSF
#' rsf_data <- amt_fisher |>
#'   filter(name == "Lupe") |>
#'   make_track(x_, y_, t_) |>
#'   random_points() |>
#'   extract_covariates(amt_fisher_covar$elevation) |>
#'   extract_covariates(amt_fisher_covar$popden) |>
#'   extract_covariates(amt_fisher_covar$landuse) |>
#'   mutate(lu = factor(landuse))
#'
#' # Fit RSF
#' m1 <- rsf_data |>
#'   fit_rsf(case_ ~ lu + elevation + popden)
#'
#' # Calculate log-RSS
#' # data.frame of x1s
#' x1 <- data.frame(lu = factor(50, levels = levels(rsf_data$lu)),
#'                  elevation = seq(90, 120, length.out = 100),
#'                  popden = mean(rsf_data$popden))
#' # data.frame of x2 (note factor levels should be same as model data)
#' x2 <- data.frame(lu = factor(50, levels = levels(rsf_data$lu)),
#'                  elevation = mean(rsf_data$elevation),
#'                  popden = mean(rsf_data$popden))
#' # Calculate (use se method for confidence interval)
#' logRSS <- log_rss(object = m1, x1 = x1, x2 = x2, ci = "se")
#'
#' # Plot
#' ggplot(logRSS$df, aes(x = elevation_x1, y = log_rss)) +
#'   geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
#'   geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "gray80") +
#'   geom_line() +
#'   xlab(expression("Elevation " * (x[1]))) +
#'   ylab("log-RSS") +
#'   ggtitle(expression("log-RSS" * (x[1] * ", " * x[2]))) +
#'   theme_bw()
#'
#' # SSF -------------------------------------------------------
#' # Fit an SSF, then calculate log-RSS to visualize results.
#'
#' # Load data
#' data(deer)
#' sh_forest <- get_sh_forest()
#'
#' # Prepare data for SSF
#' ssf_data <- deer |>
#'   steps_by_burst() |>
#'   random_steps(n = 15) |>
#'   extract_covariates(sh_forest) |>
#'   mutate(forest = factor(forest, levels = 1:0,
#'                     labels = c("forest", "non-forest")),
#'   cos_ta = cos(ta_),
#'   log_sl = log(sl_))
#'
#' # Fit an SSF (note model = TRUE necessary for predict() to work)
#' m2 <- ssf_data |>
#'   fit_clogit(case_ ~ forest + strata(step_id_), model = TRUE)
#'
#' # Calculate log-RSS
#' # data.frame of x1s
#' x1 <- data.frame(forest = factor(c("forest", "non-forest")))
#' # data.frame of x2
#' x2 <- data.frame(forest = factor("forest", levels = levels(ssf_data$forest)))
#' # Calculate
#' logRSS <- log_rss(object = m2, x1 = x1, x2 = x2, ci = "se")
#'
#' # Plot
#' ggplot(logRSS$df, aes(x = forest_x1, y = log_rss)) +
#'   geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
#'   geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.25) +
#'   geom_point(size = 3) +
#'   xlab(expression("Forest Cover " * (x[1]))) +
#'   ylab("log-RSS") +
#'   ggtitle(expression("log-RSS" * (x[1] * ", " * x[2]))) +
#'   theme_bw()
#' }
#' @export
log_rss <- function(object, ...){
  UseMethod("log_rss", object)
}

#' @rdname log_rss
#' @export
log_rss.glm <- function(object, x1, x2, ci = NA, ci_level = 0.95, n_boot = 1000, ...){

  # Check inputs
  check_log_rss_inputs(object = object,
                       x1 = x1,
                       x2 = x2,
                       ci = ci,
                       ci_level = ci_level,
                       n_boot = n_boot,
                       ...)

  # Check if it is a fit_logit
  model <- if (inherits(object, "fit_logit")) {
    object$model
  } else {
    object
  }

  # Get model terms
  Terms <- stats::delete.response(terms(model))

  # Get model matrix
  X1 <- stats::model.matrix(Terms, data = x1)
  X2 <- stats::model.matrix(Terms, data = x2)

  # Get betas
  b <- stats::coef(model)

  # Get Sigma
  S <- stats::vcov(model)


  #Calculate g(x) [ the linear predictor ]
  g_x1 <- unname((X1 %*% b)[, 1])
  g_x2 <- unname((X2 %*% b)[, 1])

  #Include values of x1 in return data.frame
  df <- x1
  names(df) <- unname(sapply(names(x1), append_x1))
  #Calculate log_rss
  df$log_rss <- g_x1 - g_x2

  #Calculate confidence intervals
  if (!is.na(ci)){
    if (ci == "se"){
      # Subtract x2 model matrix from each row of x1
      delta_X <- sweep(data.matrix(X1), 2, data.matrix(X2))

      #Get variance of log-RSS prediction
      var_pred <- diag(delta_X %*% S %*% t(delta_X))

      #Get standard error of prediction
      logrss_se <- unname(sqrt(var_pred))

      # Get critical value
      p <- 1 - ((1 - ci_level)/2)
      zstar <- qnorm(p)

      # Compute bounds
      df$lwr <- df$log_rss - zstar * logrss_se
      df$upr <- df$log_rss + zstar * logrss_se
    }
    if (ci == "boot"){
      # Bootstrap method
      cat("Generating bootstrapped confidence intervals...\n")
      boot_res <- bootstrap_logrss(object = object, x1 = x1, x2 = x2,
                                   ci_level = ci_level, n_boot = n_boot,
                                   mle = df$log_rss)

      cat(" ... finished bootstrapping.\n")
      df$lwr <- boot_res$lwr
      df$upr <- boot_res$upr
    }
  }

  #Compose the list to return
  res <- list(df = df,
              x1 = x1,
              x2 = x2,
              formula = model$formula)

  #Set the S3 class
  class(res) <- c("log_rss", class(res))

  #Return log_rss
  return(res)
}
#' @rdname log_rss
#' @export
log_rss.fit_clogit <- function(object, x1, x2, ci = NA, ci_level = 0.95, n_boot = 1000, ...) {

  if(is.null(object$model$model)){
    stop(paste("Please refit your step selection model with the argument 'model = TRUE'.\n"),
         "  The predict() method from package 'survival' requires that you store the model.\n",
         "  See the Examples under ?log_rss for a demonstration, and see Details under\n",
         "     ?survival::predict.coxph for a full explanation.")
  }

  # Calculate log-RSS using method 'log_rss.clogit'
  res <- log_rss.clogit(object = object$model,
                        x1 = x1,
                        x2 = x2,
                        ci = ci,
                        ci_level = 0.95, n_boot = 1000,
                        ...)
  return(res)
}

#' @rdname log_rss
#' @export
log_rss.clogit <- function(object, x1, x2, ci = NA, ci_level = 0.95, n_boot = 1000, ...) {

  if(is.null(object$model)){
    stop(paste("Please refit your step selection model with the argument 'model = TRUE'.\n"),
         "  The predict() method from package 'survival' requires that you store the model.\n",
         "  See the Examples under ?log_rss for a demonstration, and see Details under\n",
         "     ?survival::predict.coxph for a full explanation.")
  }


  # Check inputs
  check_log_rss_inputs(object = object,
                       x1 = x1,
                       x2 = x2,
                       ci = ci,
                       ci_level = ci_level,
                       n_boot = n_boot,
                       ...)

  #Calculate correction due to sample-centered means (see ?survival::predict.coxph for details)
  uncenter <- sum(stats::coef(object) * object$means, na.rm=TRUE)
  #predict(..., reference = "sample", se.fit = TRUE) will throw error without a "step_id_"
  #column, even though it isn't using it
  x1_dummy <- x1
  x2_dummy <- x2
  x1_dummy$step_id_ = object$model$`strata(step_id_)`[1]
  x2_dummy$step_id_ = object$model$`strata(step_id_)`[1]

  # Get model matrix
  X1 <- survival:::model.matrix.coxph(object, data = x1_dummy)
  X2 <- survival:::model.matrix.coxph(object, data = x2_dummy)

  # Get betas
  b <- stats::coef(object)

  # Get Sigma
  S <- stats::vcov(object)

  #Calculate g(x) [ the linear predictor ]
  g_x1 <- unname((X1 %*% b)[, 1])
  g_x2 <- unname((X2 %*% b)[, 1])

  #Include values of x1 in return data.frame
  df <- x1
  names(df) <- unname(sapply(names(x1), append_x1))
  #Calculate log_rss
  df$log_rss <- g_x1 - g_x2

  #Calculate confidence intervals
  if (!is.na(ci)){
    if (ci == "se"){ #Standard error method
      # Subtract x2 model matrix from each row of x1
      delta_X <- sweep(data.matrix(X1), 2, data.matrix(X2))

      #Get variance of log-RSS prediction
      var_pred <- diag(delta_X %*% S %*% t(delta_X))

      #Get standard error of prediction
      logrss_se <- unname(sqrt(var_pred))

      # Get critical value
      p <- 1 - ((1 - ci_level)/2)
      zstar <- qnorm(p)

      # Compute bounds
      df$lwr <- df$log_rss - zstar * logrss_se
      df$upr <- df$log_rss + zstar * logrss_se
    }
    if (ci == "boot") { #Bootstrap method
      cat("Generating bootstrapped confidence intervals...\n")
      cat("   Expect this to take some time... \n")
      boot_res <- bootstrap_logrss.fit_clogit(object = object, x1 = x1, x2 = x2,
                                              ci_level = ci_level, n_boot = n_boot,
                                              mle = df$log_rss)

      cat(" ... finished bootstrapping.\n")
      df$lwr <- boot_res$lwr
      df$upr <- boot_res$upr
    }
  }

  #Compose the list to return
  res <- list(df = df,
              x1 = x1,
              x2 = x2,
              formula = object$formula)

  #Set the S3 class
  class(res) <- c("log_rss", class(res))

  #Return log_rss
  return(res)
}

#' @rdname log_rss
#' @export
log_rss.glmmTMB <- function(object, x1, x2, ci = NA, ci_level = 0.95, n_boot = 1000, ...) {

  # Which random effects are present?
  REs <- RE_glmmTMB(object)

  # Warn user if there are others beside the stratum
  if (length(REs$other) > 0) {
    warning(paste0("Random effects detected:\n",
                   paste0("  * ", REs$other, "\n"),
                   "log-RSS predictions will consider only fixed effects."))
  }

  # Get model terms
  Terms <- stats::delete.response(terms(object))

  # Get model matrix
  X1 <- stats::model.matrix(Terms, data = x1)
  X2 <- stats::model.matrix(Terms, data = x2)

  # Get betas
  bb <- object$fit$par
  # Just the betas
  b <- bb[which(names(bb) == "beta")]

  # Get Sigma
  S <- stats::vcov(object)$cond

  #Calculate g(x) [ the linear predictor ]
  g_x1 <- unname((X1 %*% b)[, 1])
  g_x2 <- unname((X2 %*% b)[, 1])

  #Include values of x1 in return data.frame
  df <- x1
  names(df) <- unname(sapply(names(x1), append_x1))
  #Calculate log_rss
  df$log_rss <- g_x1 - g_x2

  #Calculate confidence intervals
  if (!is.na(ci)){
    if (ci == "se"){ #Standard error method
      # Subtract x2 model matrix from each row of x1
      delta_X <- sweep(data.matrix(X1), 2, data.matrix(X2))

      #Get variance of log-RSS prediction
      var_pred <- diag(delta_X %*% S %*% t(delta_X))

      #Get standard error of prediction
      logrss_se <- unname(sqrt(var_pred))

      # Get critical value
      p <- 1 - ((1 - ci_level)/2)
      zstar <- qnorm(p)

      # Compute bounds
      df$lwr <- df$log_rss - zstar * logrss_se
      df$upr <- df$log_rss + zstar * logrss_se
    }
    if (ci == "boot") { #Bootstrap method
      warning("Bootstrap CI method not currently implemented for glmmTMB models.")
    }
  }

  #Compose the list to return
  res <- list(df = df,
              x1 = x1,
              x2 = x2,
              formula = formula(object))

  #Set the S3 class
  class(res) <- c("log_rss", class(res))

  #Return log_rss
  return(res)
}

#' @rdname log_rss
#' @export
log_rss.gam <- function(object, x1, x2, ci = NA, ci_level = 0.95, n_boot = 1000, ...) {

  # Get model matrix
  X1 <- stats::predict(object, type = "lpmatrix", newdata = x1)
  X2 <- stats::predict(object, type = "lpmatrix", newdata = x2)

  # Get betas
  b <- stats::coef(object)

  # Get Sigmobject# Get Sigma
  S <- stats::vcov(object)

  #Calculate g(x) [ the linear predictor ]
  g_x1 <- unname((X1 %*% b)[, 1])
  g_x2 <- unname((X2 %*% b)[, 1])

  #Include values of x1 in return data.frame
  df <- x1
  names(df) <- unname(sapply(names(x1), append_x1))
  #Calculate log_rss
  df$log_rss <- g_x1 - g_x2

  #Calculate confidence intervals
  if (!is.na(ci)){
    if (ci == "se"){ #Standard error method
      # Subtract x2 model matrix from each row of x1
      delta_X <- sweep(data.matrix(X1), 2, data.matrix(X2))

      #Get variance of log-RSS prediction
      var_pred <- diag(delta_X %*% S %*% t(delta_X))

      #Get standard error of prediction
      logrss_se <- unname(sqrt(var_pred))

      # Get critical value
      p <- 1 - ((1 - ci_level)/2)
      zstar <- qnorm(p)

      # Compute bounds
      df$lwr <- df$log_rss - zstar * logrss_se
      df$upr <- df$log_rss + zstar * logrss_se
    }
    if (ci == "boot") { #Bootstrap method
      warning("Bootstrap CI method not currently implemented for mgcv models.")
    }
  }

  #Compose the list to return
  res <- list(df = df,
              x1 = x1,
              x2 = x2,
              formula = formula(object))

  #Set the S3 class
  class(res) <- c("log_rss", class(res))

  #Return log_rss
  return(res)
}


#' Internal argument checks for `log_rss`
#'
#' Blah

check_log_rss_inputs <- function(
    object,
    x1,
    x2,
    ci,
    ci_level,
    n_boot, ...) {

  # Check if it is a fit_logit or fit_clogit
  model <- if (inherits(object, c("fit_logit", "fit_clogit"))) {
    object$model
  } else {
    object
  }

  # Object checks here first

  if(!inherits(x1, "data.frame")){
    stop("'x1' should be an object of class 'data.frame'.")
  }
  if(!inherits(x2, "data.frame")){
    stop("'x2' should be an object of class 'data.frame'.")
  }
  #Check rows of x2
  if(nrow(x2) != 1L){
    stop(paste0("The data.frame provided as 'x2' should have exactly 1 row.\n",
                "  See ?log_rss for more details."))
  }

  # Check confidence interval arguments
  if(!is.na(ci)){
    if(!(ci %in% c("se", "boot"))){
      stop("'ci' should be NA or the string \"se\" or \"boot\".")
    }
    checkmate::assert_numeric(ci_level, lower = 0, upper = 1, max.len = 1)
    if(ci == "boot"){
      checkmate::assert_integerish(n_boot, lower = 1, len = 1)
    }
  }

  # If there are factor variables in model, check that levels are identical
  # in original data, x1, and x2 (internal amt function)
  check_factors(model = model, x1 = x1, x2 = x2)
}

#' Plot a `log_rss` object
#'
#' Default plot method for an object of class `log_rss`
#'
#' @param x `[log_rss]` An object returned by the function \code{\link{log_rss}()}.
#' @param x_var1 `[character]` The variable to plot on the x-axis. A string of
#' either `"guess"` (default -- see Details) or the variable name.
#' @param x_var2 `[character]` A second predictor variable to include in the plot.
#' Either `"guess"` (default -- see Details), `NA`, or the variable name.
#' @param ... `[any]` Additional arguments to be passed to `\link{plot}()`.
#' \emph{Not currently implemented}.
#'
#' @details This function provides defaults for a basic plot, but we encourage
#' the user to carefully consider how to represent the patterns found in their
#' habitat selection model.
#'
#' The function \code{\link{log_rss}()} is meant to accept a user-defined
#' input for `x1`. The structure of `x1` likely reflects how the user intended
#' to visualize the results. Therefore, it is possible to "guess" which covariate
#' the user would like to see on the x-axis by choosing the column from `x1` with
#' the most unique values. Similarly, if there is a second column with multiple
#' unique values, that could be represented by a color. Note that if the user needs
#' to specify `x_var1`, then we probably cannot guess `x_var2`. Therefore, if the
#' user specifies `x_var1 != "guess" & x_var2 == "guess"`, the function will return
#' an error.
#'
#' This function uses integers to represent colors, and therefore the user can
#' change the default colors by specifying a custom \code{\link{palette}()} before
#' calling the function.
#'
#' @return A plot.
#'
#' @examples
#' \donttest{
#' # Load data
#' data("amt_fisher")
#' amt_fisher_covar <- get_amt_fisher_covars()
#'
#' # Prepare data for RSF
#' rsf_data <- amt_fisher |>
#'   filter(name == "Leroy") |>
#'   make_track(x_, y_, t_) |>
#'   random_points() |>
#'   extract_covariates(amt_fisher_covar$landuse) |>
#'   mutate(lu = factor(landuse))
#'
#' # Fit RSF
#' m1 <- rsf_data |>
#'   fit_rsf(case_ ~ lu)
#'
#' # Calculate log-RSS
#' # data.frame of x1s
#' x1 <- data.frame(lu = sort(unique(rsf_data$lu)))
#' # data.frame of x2 (note factor levels should be same as model data)
#' x2 <- data.frame(lu = factor(140,
#' levels = levels(rsf_data$lu)))
#' # Calculate
#' logRSS <- log_rss(object = m1, x1 = x1, x2 = x2)
#'
#' # Plot
#' plot(logRSS)
#' }
#'
#' @export
plot.log_rss <- function(x, x_var1 = "guess", x_var2 = "guess", ...){
  #Check inputs
  if (!inherits(x, "log_rss")){
    stop("'x' must be of class 'log_rss'. See ?log_rss for details.")
  }
  checkmate::assert_character(x_var1)
  checkmate::assert_character(x_var2)

  #Guess x_vars if necessary
  if (x_var1 == "guess"){
    #Grab terms from the formula
    trms <- attr(terms(x$formula), "term.labels")
    #Grab x1 from the x
    x1_df <- x$x1
    #Keep only those columns that appear in the model formula
    x1_df <- x1_df[, which(names(x1_df) %in% trms), drop = FALSE]
    #Count unique values
    n_unq <- apply(x1_df, 2, function(x){length(unique(x))})
    #Sort by frequency
    n_unq <- sort(n_unq, decreasing = TRUE)
    #Assign as x_var1 the variable with the most unique values
    x_var1 <- names(n_unq)[1]

    #Check x_var2
    if (!is.na(x_var2)){
      if (x_var2 == "guess"){
        x_var2 <- names(n_unq)[2] #Returns NA if there isn't one
      }
    }

    #Inform the user of the guesses
    cat(paste0("Guessing x_vars:\n",
               "  x_var1 = ", x_var1, "\n",
               "  x_var2 = ", x_var2, "\n"))

  } else {
    #If we aren't guessing x_var1, we probably shouldn't guess x_var2.
    if (!is.na(x_var2)){
      if (x_var2 == "guess"){
        stop("If the user specifies 'x_var1', they must also specify 'x_var2'.")
      }
    }
  }

  #x$df has "_x1" appended to the names -- update x_vars to match
  x_var1_x1 <- append_x1(x_var1)
  x_var2_x1 <- append_x1(x_var2)

  #Decide whether plot should be lines or points
  cl_var1 <- class(x$df[[x_var1_x1]])
  if (cl_var1 == "numeric"){
    plot_type = "l"
  } else {
    plot_type = "p"
  }

  #Add color column
  if (is.na(x_var2)){
    #If there is no second x variable, color is constant
    x$df$color <- 1
  } else {
    #If there is, choose colors based on levels of x_var2
    x$df$color <- as.integer(factor(x$df[[x_var2_x1]]))
  }

  #Now plot
  if (plot_type == "p"){
    #If plotting points, we can plot them all at once
    #Note, fill = color for the case where x1 is a factor
    graphics::plot(x = x$df[[x_var1_x1]], y = x$df$log_rss,
                   type = plot_type, col = x$df$color, fill = x$df$color,
                   xlab = x_var1, ylab = "log-RSS",
                   main = expression("log-RSS(" * x[1] * " vs. " * x[2]*")"))
  } else {
    #If plotting lines, might have to separate by color
    if (max(x$df$color) > 1){
      #Setup plot
      graphics::plot(x = x$df[[x_var1_x1]], y = x$df$log_rss,
                     type = plot_type, col = par()$bg,
                     xlab = x_var1, ylab = "log-RSS",
                     main = expression("log-RSS(" * x[1] * " vs. " * x[2]*")"))
      for (i in 1:max(x$df$color)){
        df_sub <- x$df[which(x$df$color == i),]
        lines(x = df_sub[[x_var1_x1]], y = df_sub$log_rss, col = i)
      }
    } else {
      graphics::plot(x = x$df[[x_var1_x1]], y = x$df$log_rss,
                     type = plot_type, col = x$df$color,
                     xlab = x_var1, ylab = "log-RSS",
                     main = expression("log-RSS(" * x[1] * " vs. " * x[2]*")"))
    }
  }
}

#' Append "_x1"
#'
#' Helper function to append `_x1` to variable names
#'
#' @param string `[character]` Variable name to possibly append to
#'
#' @details The function first checks if "_x1" is already appended and adds it if
#' it is not. This is meant for internal use in `\link{plot.log_rss}()`.
#' @return A string.
#' @keywords internal

append_x1 <- function(string){
  #If string is NA, return NA
  if (is.na(string)){
    return(NA)
  }
  #Check if the name already ends in "_x1"
  l <- nchar(string)
  last3 <- substr(string, start = (l-2), stop = l)
  if (last3 == "_x1"){
    return(string)
  } else {
    #Check if name ends in just "_"
    last1 <- substr(string, start = l, stop = l)
    if (last1 == "_"){
      string <- paste0(string, "x1")
    } else {
      string <- paste0(string, "_x1")
    }
    return(string)
  }
}

#' Check factor levels
#'
#' Check factor levels before log-RSS calculation
#'
#' @param model `[glm, clogit]` \cr The model object from a fitted RSF/(i)SSF.
#' *I.e.*, it will be `object$model` when called within `log_rss()`.
#' @param x1 `[data.frame]` \cr A `data.frame` representing the habitat values
#' at location x_1. Must contain all fitted covariates as expected by
#' `predict()`.
#' @param x2 `[data.frame]` \cr A 1-row `data.frame` representing the single
#' hypothetical location of x_2. Must contain all fitted covariates as expected
#' by `predict()`.
#'
#' @details This function is meant for internal use by `log_rss()` and is
#' not meant to be called by the user.
#'
#' @keywords internal
#'
#'
check_factors <- function(model, x1, x2){
  # Get original data from model
  dat <- model$model

  # Get factors from contrasts object
  fcts <- names(model$contrasts)

  # Nothing to check if there aren't any factors
  if (is.null(fcts)){
    return(NULL)
  }

  # Check each factor
  f_list <- lapply(fcts, function(f) {

    # Compare data to x1
    dat_x1 <- identical(levels(dat[[f]]), levels(x1[[f]]))
    # Compare data to x2
    dat_x2 <- identical(levels(dat[[f]]), levels(x2[[f]]))
    # Compare x1 to x2
    x1_x2 <- identical(levels(x1[[f]]), levels(x2[[f]]))

    # Check for any errors
    if (all(c(dat_x1, dat_x2, x1_x2))){
      # No errors, error string should be NA
      es <- NA
    } else {
      # Start error string with factor name
      es <- paste0("\nError with factor \"", f, "\": ")

      # Add detail for comparisons that fail
      if (! dat_x1) {
        es <- paste0(es, "\n  * levels from data do not match levels from x1.")
      }
      if (! dat_x2) {
        es <- paste0(es, "\n  * levels from data do not match levels from x2.")
      }
      if (! x1_x2) {
        es <- paste0(es, "\n  * levels from x1 do not match levels from x2.")
      }
    }
    return(es)
  })

  # If all elements of f_list are NA, then no errors
  if (length(na.omit(unlist(f_list))) == 0){
    return(NULL)
  } else {
    error_text <- paste(f_list, collapse = "\n\n")
    stop("factor levels do not match.\n", error_text)
  }
}

#' Bootstrap log-RSS estimate
#'
#' Use empirical bootstrap to estimate log-RSS CI
#'
#' @details This function is meant for internal use by `log_rss()` and is
#' not meant to be called by the user.
#'
#' @rdname bootstrap_logrss
#' @keywords internal
bootstrap_logrss <- function(object, ...){

  warning("For amt version <= 0.3.0.0, bootstrapped confidence intervals ",
          "were created using empirical bootstrapping ",
          "(resampling the data). In later versions, these confidence ",
          "intervals are created using parametric bootstrapping ",
          "(resampling the coefficients from a multivariate normal ",
          "distribution). Results should be similar, but may not ",
          "be exact.")

  UseMethod("bootstrap_logrss", object)
}

#' @rdname bootstrap_logrss
bootstrap_logrss.glm <- function(object, x1, x2, ci_level, n_boot, mle, ...){
  #Perform the bootstrap
  arr <- replicate(n_boot, boot1.glm(object, x1, x2), simplify = "array")
  #Lower percentile
  p_lwr <- (1 - ci_level)/2
  #Upper percentile
  p_upr <- 1 - p_lwr
  #Return sample quantiles
  q_lwr <- apply(arr, 1, quantile, p_lwr)
  q_upr <- apply(arr, 1, quantile, p_upr)
  #Return sample mean
  boot_mean <- apply(arr, 1, mean)
  #Distance to lower and upper
  d_lwr <- q_lwr - boot_mean
  d_upr <- q_upr - boot_mean
  #Calculate CI
  res <- data.frame(lwr = mle + d_lwr,
                    upr = mle + d_upr)
  #Return
  return(res)
}

#' @rdname bootstrap_logrss
bootstrap_logrss.fit_clogit <- function(object, x1, x2, ci_level, n_boot, mle, ...){
  #Perform the bootstrap
  arr <- replicate(n_boot, boot1.fit_clogit(object, x1, x2), simplify = "array")
  #Lower percentile
  p_lwr <- (1 - ci_level)/2
  #Upper percentile
  p_upr <- 1 - p_lwr
  #Return sample quantiles
  q_lwr <- apply(arr, 1, quantile, p_lwr)
  q_upr <- apply(arr, 1, quantile, p_upr)
  #Return sample mean
  boot_mean <- apply(arr, 1, mean)
  #Distance to lower and upper
  d_lwr <- q_lwr - boot_mean
  d_upr <- q_upr - boot_mean
  #Calculate CI
  res <- data.frame(lwr = mle + d_lwr,
                    upr = mle + d_upr)
  #Return
  return(res)
}


#' Single bootstrap iteration
#'
#' Runs a single iteration of the empirical bootstrap
#'
#' @details This function is meant for internal use by `bootstrap_logrss()` and
#' is not meant to be called by the user.
#' @rdname boot1
#' @keywords internal
#' @export
boot1.glm <- function(object, x1, x2){
  # not so nice workaround
  object <- if (inherits(object, "fit_logit")) {
    object$model
  } else {
    object
  }
  dat <- object$model
  w <- stats::weights(object)

  #If resampling factor levels, missing levels can cause prediction to fail
  logrss <- NULL
  i <- 1
  while(is.null(logrss)){
    #Resample
    wp <- sample(1:nrow(dat), nrow(dat), replace = TRUE)
    newdat <- dat[wp, ]
    newweights <- w[wp]
    #Refit model
    m <- stats::glm(formula = formula(object), data = newdat,
                    family = stats::binomial())#,
    #weights = rep(1, nrow(newdat)))
    try({logrss <- log_rss(m, x1, x2, ci = NA)$df$log_rss}, silent = TRUE)
    i <- i + 1
    #Warn after 25 tries
    if (i == 25){
      cat(paste("\n   Warning... Having trouble resampling a valid dataset.",
                "This may occur if you have very few observations of a level",
                "of a factor variable."))
    }
    #Quit after 50 tries (consider giving user control over this)
    if (i == 50){
      stop(paste("Bootstrapping failed. This may occur if you have very few",
                 "observations of a level of a factor variable."))
    }
  }
  return(logrss)
}

#' @rdname boot1
#' @export
boot1.fit_clogit <- function(object, x1, x2){
  dat <- object$model$model
  #If resampling factor levels, missing levels can cause prediction to fail
  logrss <- NULL
  i <- 1
  while(is.null(logrss)){
    #Resample
    newdat <- resample_coxph(dat)
    #Refit model
    rhs <- as.character(formula(object$model))[3]
    m <- fit_clogit(formula = reformulate(rhs, response = "case_"), data = newdat, model = TRUE)
    try({logrss <- log_rss(m, x1, x2, ci = NA)$df$log_rss}, silent = TRUE)
    i <- i + 1
    #Warn after 25 tries
    if (i == 25){
      cat(paste("\n   Warning... Having trouble resampling a valid dataset.",
                "This may occur if you have very few observations of a level",
                "of a factor variable."))
    }
    #Quit after 50 tries (consider giving user control over this)
    if (i == 50){
      stop(paste("Bootstrapping failed. This may occur if you have very few",
                 "observations of a level of a factor variable."))
    }
  }
  return(logrss)
}

resample_coxph <- function(mdat){
  #Reformat
  names(mdat)[1] <- "case_"
  mdat$case_ <- as.character(mdat$case_) == "1"
  names(mdat)[which(names(mdat) == "strata(step_id_)")] <- "step_id_"
  mdat$step_id_ <- as.character(mdat$step_id_)
  mdat$step_id_ <- unlist(
    lapply(
      strsplit(mdat$step_id_, "=", fixed = TRUE), getElement, 2))
  #Create new data.frame with just columns to drop attributes
  dat <- mdat[, 1:ncol(mdat)]

  #Sampling only strata
  n_obs <- sum(dat$case_)
  strats <- sample(x = unique(dat$step_id_), size = n_obs, replace = TRUE)
  new_dat_list <- lapply(strats, function(x){
    dat[which(dat$step_id_ == x),]
  })
  new_dat <- dplyr::bind_rows(new_dat_list)
  #Return
  return(new_dat)
}
