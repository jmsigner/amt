# These functions enable support for various model types (fitting methods)
# for RSFs and (i)SSFs

#' Currently supported models
#'
#' Prints currently supported models
#'
#' @author Brian J. Smith
#'
#' @return Returns list of possible RSF and (i)SSF models invisibly.
#'
#' @examples
#'
#' supported_models()
#'
#'
#' @export
supported_models <- function(){

  mods <- list(
    rsf = c("stats::glm",
            # "lme4::glmer (wishlist)",
            "glmmTMB::glmmTMB (in development)",
            "mgcv::gam"),
    ssf = c("survival::clogit",
            "glmmTMB::glmmTMB (in development)",
            "mgcv::gam")
  )

  # Currently supported models for fitting RSFs:
  cat("\n")
  cat("===========================================================\n")
  cat("Currently supported models (fitting methods)\n")
  cat("===========================================================\n")
  cat("RSFs (aka, HSFs) \n")
  cat("---------------- \n")
  cat(paste0("  * ", mods$rsf, "\n"), sep = "")
  cat("===========================================================\n")
  cat("(i)SSFs \n")
  cat("-------- \n")
  cat(paste0("  * ", mods$ssf, "\n"), sep = "")
  cat("===========================================================\n")

  return(invisible(mods))
}

# Return linear predictor ----
#' Return linear predictor
#'
#' Returns the linear predictor for any supported model type
#'
#' @inheritParams log_rss
#' @param newdata `[data.frame]` \cr A `data.frame`. Must contain all fitted
#' covariates as expected by `predict()`.
#'
#' @details
#' For internal use in calculating log-RSS.
#'
#'
#' @author Brian J. Smith
#'
#' @rdname linear_predictor
#' @export
linear_predictor <- function(object, newdata, ...) {
  UseMethod("linear_predictor", object)
}

#' @rdname linear_predictor
#' @export
linear_predictor.glm <- function(object, newdata, ...) {

  # Get model terms
  Terms <- stats::delete.response(terms(object))

  # Get model matrix
  X <- stats::model.matrix(Terms, data = newdata)

  # Get betas
  b <- stats::coef(object)

  #Calculate g(x) [ the linear predictor ]
  g <- unname((X %*% b)[, 1])

  # Return
  return(g)
}

#' @rdname linear_predictor
#' @export
linear_predictor.clogit <- function(object, newdata, ...) {

  #predict() will throw error without a "step_id_"
  #column, even though it isn't using it
  newdata_dummy <- newdata
  newdata_dummy$step_id_ <- object$model$`strata(step_id_)`[1]

  # Calculate g(x)
  g <- survival:::predict.coxph(object = object, newdata = newdata_dummy,
                                type = "lp", reference = "zero")

  # Return
  return(g)
}

#' @rdname linear_predictor
#' @export
linear_predictor.gam <- function(object, newdata, ...) {

  # Calculate g(x)
  g <- mgcv::predict.gam(object, newdata = newdata,
                         type = "link")

  # Return
  return(as.vector(g))
}

# Return model matrix ----
#' Return model matrix
#'
#' Returns model matrix for `newdata`
#'
#' @inheritParams linear_predictor
#'
#' @details
#' For internal use in calculating log-RSS.
#'
#'
#' @author Brian J. Smith
#'
#' @rdname model_matrix
#' @export
model_matrix <- function(object, newdata, ...) {
  UseMethod("model_matrix", object)
}

#' @rdname model_matrix
#' @export
model_matrix.glm <- function(object, newdata, ...) {
  # Get model terms
  Terms <- stats::delete.response(terms(object))

  # Get model matrices
  X <- stats::model.matrix(Terms, data = newdata)

  # Return
  return(X)
}

#' @rdname model_matrix
#' @export
model_matrix.clogit <- function(object, newdata, ...) {
  #predict() will throw error without a "step_id_"
  #column, even though it isn't using it
  newdata_dummy <- newdata
  newdata_dummy$step_id_ <- object$model$`strata(step_id_)`[1]

  # Get model matrix
  X <- survival:::predict.coxph(object, newdata = newdata_dummy,
                                type = "terms", reference = "zero")

  # Return
  return(newdata_dummy)
}

#' @rdname model_matrix
#' @export
model_matrix.gam <- function(object, newdata, ...) {

  # Get model matrices
  X <- mgcv::predict.gam(object, newdata = newdata, type = "lpmatrix")

  # Return
  return(X)
}

# Return difference matrix ----
#' Return difference matrix
#'
#' Subtracts matrix `x2` from all rows of matrix `x1`
#'
#' @inheritParams log_rss
#'
#' @details
#' For internal use in calculating log-RSS.
#'
#'
#' @author Brian J. Smith
#'
#' @rdname diff_matrix
#' @export
diff_matrix <- function(object, x1, x2, ...) {
  UseMethod("diff_matrix", object)
}

#' @rdname diff_matrix
#' @export
diff_matrix.glm <- function(object, x1, x2, ...) {
  # Get model terms
  Terms <- stats::delete.response(terms(object))

  # Get model matrices
  X1 <- stats::model.matrix(Terms, data = x1)
  X2 <- stats::model.matrix(Terms, data = x2)

  # Subtract x2 model matrix from each row of x1
  delta_X <- sweep(data.matrix(X1), 2, data.matrix(X2))

  # Return
  return(delta_X)
}

#' @rdname diff_matrix
#' @export
diff_matrix.clogit <- function(object, x1, x2, ...) {
  #predict() will throw error without a "step_id_"
  #column, even though it isn't using it
  x1_dummy <- x1
  x1_dummy$step_id_ <- object$model$`strata(step_id_)`[1]
  x2_dummy <- x2
  x2_dummy$step_id_ <- object$model$`strata(step_id_)`[1]

  # Get model matrices
  X1 <- survival:::predict.coxph(object, newdata = x1_dummy,
                                 type = "terms", reference = "zero")
  X2 <- survival:::predict.coxph(object, newdata = x2_dummy,
                                 type = "terms", reference = "zero")

  # Subtract x2 model matrix from each row of x1
  delta_X <- sweep(data.matrix(X1), 2, data.matrix(X2))

  # Return
  return(delta_X)
}

#' @rdname diff_matrix
#' @export
diff_matrix.gam <- function(object, x1, x2, ...) {

  # Get model matrices
  X1 <- mgcv::predict.gam(object, newdata = x1, type = "lpmatrix")
  X2 <- mgcv::predict.gam(object, newdata = x2, type = "lpmatrix")

  # Subtract x2 model matrix from each row of x1
  delta_X <- sweep(data.matrix(X1), 2, data.matrix(X2))

  # Return
  return(delta_X)
}

# Return variance-covariance matrix ----
#' Return variance-covariance matrix
#'
#' Returns a variance-covariance matrix for any supported model type
#'
#' @inheritParams log_rss
#'
#' @details
#' The `default` method is confirmed to work for:
#'    * `glm`
#'    * `clogit`
#'    * `gam`
#'
#'
#' @rdname get_Sigma
#' @export
get_Sigma <- function(object, ...) {
  UseMethod("get_Sigma", object)
}

#' @rdname get_Sigma
#' @export
get_Sigma.default <- function(object, ...) {
  # Get Sigma
  S <- stats::vcov(object)

  # Return
  return(S)
}

# Resample betas ----
#' Resample betas
#'
#' Resamples betas for parametric bootstrap for any supported model type
#'
#' @inheritParams log_rss
#'
#' @details
#' The `default` method is confirmed to work for:
#'    * `glm`
#'    * `clogit`
#'    * `gam` (not tested with actual splines yet)
#'
#'
#' @rdname resample_betas
#' @export
resample_betas <- function(object, ...) {
  UseMethod("resample_betas", object)
}

#' @rdname resample_betas
#' @export
resample_betas.default <- function(object, ...) {
  # Get betas
  b <- stats::coef(object)

  # Get Sigma
  S <- stats::vcov(object)

  # Resample betas from multivariate normal distribution
  bb <- as.vector(mvtnorm::rmvnorm(n = 1, mean = b, sigma = S))
  names(bb) <- names(b)

  # Return
  return(bb)
}

#' @rdname resample_betas
#' @export
resample_betas.fit_clogit <- function(object, ...) {
  # Resample betas from model object
  bb <- resample_betas.defaul(object$model)

  # Return
  return(bb)
}


