#' Create UHC Plots for a Fitted Model
#'
#' Creates used-habitat calibration plots
#'
#' @param object `[fit_logit, fit_clogit]` \cr A fitted RSF or (i)SSF model.
#' Should be fit to *training* dataset separate from the testing data.
#' @param test_dat `[data.frame]` \cr A `data.frame` with *testing* data from
#' which to sample test points. Should be separate from the data used to train
#' the model passed to `object`.
#' @param n_samp `[numeric = 1000]` A `vector` of length 1 giving the number of
#' samples to use to characterize the used habitat distribution under the model.
#' @param verbose `[logical]` Should messages be displayed (`TRUE`) or not
#' (`FALSE`)?
#'
#' @details TBD
#'
#' @return Returns a `list` of class `uhc_sim` with elements:
#' - `el1`: TBD
#'
#' @seealso See Fieberg \emph{et al.} 2018 for details about UHC plots.
#'
#' Default plotting method available: \code{\link{plot.uhc_sim}()}
#'
#' @references
#' Fieberg, J.R., Forester, J.D., Street, G.M., Johnson, D.H., ArchMiller, A.A.,
#' and Matthiopoulos, J. 2018. Used-habitat calibration plots: A new procedure
#' for validating species distribution, resource selection, and step-selection
#' models. *Ecography* 41:737–752.
#'
#' @examples
#'
#' \donttest{
#'
#' # Load data
#' data(uhc_hsf_locs)
#' data(uhc_hab)
#'
#' #
#'
#' }
#' @export
prep_uhc <- function(object, test_dat, n_samp = 1000, verbose = TRUE) {

  # Check inputs
  ## object
  if(!inherits(object, c("glm", "fit_logit", "fit_clogit"))){
    stop(paste0("'object' should be an object of class 'glm', 'fit_logit', ",
                "or 'fit_clogit'."))
  }

  ## test_dat
  checkmate::assert_data_frame(test_dat)

  ## n_samp
  checkmate::assert_integerish(n_samp, lower = 1)

  # Pass to correct method
  UseMethod("prep_uhc", object)
}

#' @rdname prep_uhc
#' @export
prep_uhc.glm <- function(object, test_dat, n_samp = 1000, verbose = TRUE) {

  # Prep test_dat list
  l <- prep_test_dat(object, test_dat, verbose = verbose)

  # Steps follow steps in Fieberg et al. 2018

  # Step 1. Summarize distribution of covariates
  dist_dat <- lapply(1:length(l$vars), function(ii){
    return(ua_distr(name = l$vars[ii], data = l$data,
                    type = l$type[ii], resp = l$resp))
  })

  names(dist_dat) <- l$vars

  # Step 2. Fit a model to the training data
  # We've already fit the model, but we need the coefficient estimates and
  # variance-covariance matrix here.
  b <- coef(object)
  S <- vcov(object)

  # Step 3. Create predicted distribution
  pred_dist <- lapply(1:n_samp, function(i) {
    # Report progress
    if (verbose) {
      if (i == 1){
        cat("Sampling...\n")
      }
      cat(i, "of", n_samp, "    \r")
    }

    ### Step 3a. Draw random values for the betas
    bb <- MASS::mvrnorm(n = 1, mu = b, Sigma = S)

    ### Step 3b. Select points from test data
    # Calculate w(x) under the model
    ww <- calc_w(f = formula(object), b = bb, newdata = l$data)

    # Normalize for weights
    WW <- ww/sum(ww)

    # How many points to sample? One for each used point.
    nn <- sum(l$data[[l$resp]])

    # Sample
    rr <- sample.int(n = nrow(test_dat), size = nn, replace = TRUE,
                     prob = WW)

    # Get resulting rows from test_dat
    dd <- test_dat[rr, ]

    ### Step 3c. Summarize covariates at these locations
    # All of 'dd' should be treated as 'used' here
    dd[[l$resp]] <- TRUE
    cov_dens <- lapply(1:length(l$vars), function(ii){
      # Summarize distributions
      xx <- ua_distr(name = l$vars[ii], data = dd,
                     type = l$type[ii], resp = l$resp,
                     avail = FALSE)
      # Label iteration
      xx$iter <- i
      # Rearrange
      yy <- xx[, c("iter", "x", "y")]
      # Return from covariate lapply
      return(yy)
    })

    names(cov_dens) <- l$vars

    # Return from lapply over samples
    return(cov_dens)
  })

  # Now rearrange to combine all iterations for each covariate
  pred_dist_cov <- lapply(l$vars, function(v) {
    res <- do.call(rbind, lapply(pred_dist, getElement, v))
    return(res)
  })

  names(pred_dist_cov) <- l$vars

  # Construct list to return
  ll <- list(orig = dist_dat,
             samp = pred_dist_cov)

  # Assign class
  class(ll) <- c("uhc_data", class(ll))

  # Return
  return(ll)
}

#' Prepares `test_dat` for `uch_prep()`
#'
#' Internal function to check and format `test_dat`
#'
#' @param object `[fit_logit, fit_clogit]` \cr A fitted RSF or (i)SSF model.
#' Should be fit to *training* dataset separate from the testing data.
#' @param test_dat `[data.frame]` \cr A `data.frame` with *testing* data from
#' which to sample test points. Should be separate from the data used to train
#' the model passed to `object`.
#' @param verbose `[logical]` Should messages be displayed (`TRUE`) or not
#' (`FALSE`)?
#'
prep_test_dat <- function(object, test_dat, verbose = TRUE) {
  UseMethod("prep_test_dat", object)
}

prep_test_dat.glm <- function(object, test_dat, verbose = TRUE) {

  ## Check that 'test_dat' is consistent with 'object' formula.
  # Extract formula
  f <- formula(object)

  # Check that all predictor variables appear in 'test_dat'
  preds <- all.vars(delete.response(terms(f)))
  if (any(!(preds %in% names(test_dat)))){
    stop("All variables in fitted model must appear in 'test_dat'.")
  }

  # Get response variable
  resp <- as.character(as.list(f)[[2]])

  # Check that response variable exists in test_dat
  if (is.null(test_dat[[resp]])) {
    stop("You must include the response variable in 'test_dat'.")
  }

  # Check that response variable has 0s and 1s
  if (!any(test_dat[[resp]] == 0)) {
    stop("You must include background locations (response = 0) in 'test_dat'.")
  }
  if (!any(test_dat[[resp]] == 1)) {
    stop("You must include used locations (response = 1) in 'test_dat'.")
  }

  # Columns to delete (coordinates, time, and response)
  del <- c("x_", "y_", "t_", resp)
  # Remove them
  test_dat2 <- test_dat
  test_dat2[, del] <- NULL

  ## Now assuming all columns in 'test_dat2' should be included in UHC plot
  # Extract class of each column
  cl <- sapply(test_dat2, class)

  # None should be character
  if (any(cl == "character")) {

    chr <- names(cl[cl == "character"])

    stop(paste0("All columns passed to 'test_dat' should be numeric or factor. ",
                "Please convert the following character columns to factor or ",
                "remove them from 'test_dat':\n   ",
                paste(chr, collapse = ", ")))
  }

  # Extract factor columns
  fac <- names(cl[cl == "factor"])

  # Assuming all others can be treated as numeric (e.g., logical)
  num <- names(cl[cl != "factor"])

  # Report
  if (verbose) {
    message(paste0(
      "\nThese variables in 'test_dat' will be treated as numeric: \n   ",
      paste(num, collapse = ", "), "\n",
      "\nThese variables in 'test_dat' will be treated as categorical: \n   ",
      paste(fac, collapse = ", "), "\n"
    ))

    # Types of each variable
    vars <- names(test_dat2)
    type <- ifelse(vars %in% fac, "factor", "numeric")
    names(type) <- vars

    # Construct list to return
    l <- list(data = test_dat,
              vars = vars,
              type = type,
              resp = resp)

    # Return
    return(l)
  }
}

#' Summarize distribution of used and available
#'
#' Summarizes distribution of numeric or factor variables
#'
#' @param name `[character]` Name of the column to summarize.
#' @param type `[character]` Either `"numeric"` or `"factor"` as returned by
#' \code{\link{prep_test_dat}()}.
#' @param data `[data.frame]` The `data.frame` containing the columns and the
#' response variable.
#' @param resp `[character]` Name of the response variable.
#' @param avail `[logical]` Should distribution be calculated for the available
#' locations? Defaults to `TRUE`, but should be false when summarizing the
#' bootstrapped "used" samples.
#'
ua_distr <- function(name, type, data, resp, avail = TRUE) {
  u <- data[[name]][which(data[[resp]] == 1)]
  a <- data[[name]][which(data[[resp]] == 0)]

  # If 'type' is numeric
  if (type == "numeric") {
    f_u <- stats::density(u)
    f_u_df <- data.frame(x = f_u$x, y = f_u$y, dist = "U")
    if (avail) {
      f_a <- stats::density(a)
      f_a_df <- data.frame(x = f_a$x, y = f_a$y, dist = "A")
      df <- rbind(f_u_df, f_a_df)
    } else {
      df <- f_u_df
    }
  } else {
    f_u <- table(u)/length(u)
    f_u_df <- data.frame(x = names(f_u), y = as.numeric(f_u), dist = "U")
    if (avail) {
      f_a <- table(a)/length(a)
      f_a_df <- data.frame(x = names(f_a), y = as.numeric(f_a), dist = "A")
      df <- rbind(f_u_df, f_a_df)
    } else {
      df <- f_u_df
    }
    # Restore factor levels
    df$x <- factor(df$x, levels = levels(data[[name]]))
  }

  return(df)
}

#' Calculate `w(x)`
#'
#' Calculates the value of the exponential habitat selection function
#'
#' @param f `[formula]` \cr A model formula.
#' @param b `[numeric]` A named vector of coefficients.
#' @param newdata `[data.frame]` \cr A `data.frame` to predict eHSF values.
#'
calc_w <- function(f, b, newdata) {
  # Get terms object from formula
  Terms <- stats::delete.response(terms(f))

  # Create data matrix
  X <- stats::model.matrix(Terms, data = newdata)

  # Linear combination
  l <- X %*% b

  # Subtract intercept
  g <- l[, 1] - b[["(Intercept)"]]

  # Exponentiate
  w <- exp(g)

  # Return
  return(w)
}
