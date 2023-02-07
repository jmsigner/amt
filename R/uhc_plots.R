#' Prepare Data for UHC Plots for a Fitted Model
#'
#' Creates data used to make used-habitat calibration plots
#'
#' @param object `[glm, fit_logit, fit_clogit]` \cr A fitted RSF or (i)SSF model.
#' Should be fit to *training* dataset separate from the testing data.
#' @param test_dat `[data.frame]` \cr A `data.frame` with *testing* data from
#' which to sample test points. Should be separate from the data used to train
#' the model passed to `object`.
#' @param n_samp `[numeric = 1000]` A `vector` of length 1 giving the number of
#' samples to use to characterize the used habitat distribution under the model.
#' @param n_dens `[numeric = 512]` A `numeric` vector of length 1 giving the
#' number of equally spaced points at which density (used, available, and
#' sampled) is estimated. Passed to `stats::density.default()`, which indicates
#' that `n` should usually be specified as a power of 2.
#' @param verbose `[logical]` Should messages be displayed (`TRUE`) or not
#' (`FALSE`)?
#'
#' @details This function performs the heavy lifting of creating UHC plots.
#' It creates the data used later by the `plot()` method, which actually
#' draws the UHC plots. This function (1) creates density plots of the used
#' and available locations from the *test* data, and (2) resamples the (a)
#' fitted coefficients and (b) test data (weighted by the exponential habitat
#' selection function) to create the distribution of used habitat under the
#' model.
#'
#' Note that `test_dat` should contain at least all of the variables that
#' appear in the model `object`. Any further habitat variables in `test_dat`
#' will also have UHC plots generated, treating these variables as possible
#' candidate variables that are simply not included in this particular model.
#'
#' @author Brian J. Smith
#'
#' @return Returns a `list` of class `uhc_data` with elements:
#' - `orig`: List of `data.frame`s, one per variable (see `vars`). Each
#' `data.frame` contains the density plot data (`x` and `y`) for the original
#' used (`dist == "U"`) and available (`dist == "A"`) data.
#' - `samp`: List of `data.frame`s, one per variable (see `vars`). Each
#' `data.frame` contains the density plot data (`x` and `y`) for each iteration
#' of bootstrap resampling (`iter`).
#' - `vars`: Character vector with names of the habitat variables for which to
#' create UHC plots.
#' - `type`: Named character vector with the type for each of `vars` (either
#' `"numeric"` or `"factor"`).
#' - `resp`: Character vector of length 1 with the name of the response
#' variable.
#'
#' @seealso See Fieberg \emph{et al.} 2018 for details about UHC plots.
#'
#' Default plotting method available: \code{\link{plot.uhc_data}()}
#' Coercion to `data.frame`: \code{\link{as.data.frame.uhc_data}()}
#' Subsetting method: \code{\link{`[.uhc_data`}}
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
#' # Load packages
#' library(amt)
#' library(dplyr)
#' library(terra)
#' library(sf)
#'
#' # HSF ----------------------------------------------
#' # Load data
#' data(uhc_hsf_locs)
#' data(uhc_hab)
#' hab <- rast(uhc_hab, type = "xyz", crs = "epsg:32612")
#' # Convert "cover" layer to factor
#' levels(hab[[4]]) <- data.frame(id = 1:3,
#'                                cover = c("grass", "forest", "wetland"))
#'
#' # Split into train (80%) and test (20%)
#' set.seed(1)
#' uhc_hsf_locs$train <- rbinom(n = nrow(uhc_hsf_locs),
#'                              size = 1, prob = 0.8)
#' train <- uhc_hsf_locs[uhc_hsf_locs$train == 1, ]
#' test <- uhc_hsf_locs[uhc_hsf_locs$train == 0, ]
#'
#' # Available locations
#' avail_train <- random_points(st_as_sf(st_as_sfc(st_bbox(hab))),
#'                              n = nrow(train) * 10)
#'
#' avail_test <- random_points(st_as_sf(st_as_sfc(st_bbox(hab))),
#'                             n = nrow(test) * 10)
#'
#' # Combine with used
#' train_dat <- train |>
#'   make_track(x, y, crs = 32612) |>
#'   mutate(case_ = TRUE) |>
#'   bind_rows(avail_train) |>
#'   # Attach covariates
#'   extract_covariates(hab) |>
#'   # Assign large weights to available
#'   mutate(weight = case_when(
#'     case_ ~ 1,
#'     !case_ ~ 5000
#'   ))
#'
#' test_dat <- test |>
#'   make_track(x, y, crs = 32612) |>
#'   mutate(case_ = TRUE) |>
#'   bind_rows(avail_test) |>
#'   # Attach covariates
#'   extract_covariates(hab) |>
#'   # Assign large weights to available
#'   mutate(weight = case_when(
#'     case_ ~ 1,
#'     !case_ ~ 5000
#'   ))
#'
#' # Fit (correct) HSF
#' hsf1 <- glm(case_ ~ forage + temp + I(temp^2) + pred + cover,
#'             data = train_dat, family = binomial(), weights = weight)
#'
#' # Drop weights from 'test_dat'
#' test_dat$weight <- NULL
#'
#' # Prep UHC plots
#' uhc_dat <- prep_uhc(object = hsf1, test_dat = test_dat,
#'                     n_samp = 500, verbose = TRUE)
#'
#' # Plot all variables
#' plot(uhc_dat)
#'
#' # Plot only first variable
#' plot(uhc_dat[1])
#'
#' # Plot only "cover" variable
#' plot(uhc_dat["cover"])
#'
#' # Coerce to data.frame
#' df <- as.data.frame(uhc_dat)
#'
#' # Simplify sampled lines to confidence envelopes
#' conf <- conf_envelope(df)
#'
#' # Default plot for the envelopes version
#' plot(conf)
#' }
#'
#' @export
prep_uhc <- function(object, test_dat,
                     n_samp = 1000, n_dens = 512,
                     verbose = TRUE) {

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

  ## n_dens
  checkmate::assert_integerish(n_dens, lower = 2)

  # Pass to correct method
  UseMethod("prep_uhc", object)
}

#' @rdname prep_uhc
#' @export
prep_uhc.glm <- function(object, test_dat,
                         n_samp = 1000, n_dens = 512,
                         verbose = TRUE) {

  # Prep test_dat list
  l <- prep_test_dat(object, test_dat, verbose = verbose)

  # Steps follow steps in Fieberg et al. 2018

  # Step 1. Summarize distribution of covariates
  dist_dat <- lapply(1:length(l$vars), function(ii){
    return(ua_distr(name = l$vars[[ii]], data = l$data,
                    lims = l$lims[[ii]], type = l$type[[ii]],
                    resp = l$resp, n_dens = n_dens))
  })

  names(dist_dat) <- l$vars

  # Step 2. Fit a model to the training data
  # We've already fit the model, but we need the coefficient estimates and
  # variance-covariance matrix here.
  b <- coef(object)
  S <- stats::vcov(object)

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
    ww <- calc_w(f = stats::formula(object), b = bb, newdata = l$data)

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
      xx <- ua_distr(name = l$vars[[ii]], data = dd, lims = l$lims[[ii]],
                     type = l$type[[ii]], resp = l$resp, n_dens = n_dens,
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
             samp = pred_dist_cov,
             vars = l$vars,
             lims = l$lims,
             type = l$type,
             resp = l$resp)

  # Assign class
  class(ll) <- c("uhc_data", class(ll))

  # Return
  return(ll)
}

#' @rdname prep_uhc
#' @export
prep_uhc.fit_logit <- function(object, test_dat, n_samp = 1000, verbose = TRUE) {
  prep_uhc.glm(object = object$model, test_dat = test_dat,
               n_samp = n_samp, verbose = verbose)
}

#' @rdname prep_uhc
#' @export
prep_uhc.fit_clogit <- function(object, test_dat, n_samp = 1000, verbose = TRUE) {

  # Prep test_dat list
  l <- prep_test_dat(object, test_dat, verbose = verbose)

  # Steps follow steps in Fieberg et al. 2018

  # Step 1. Summarize distribution of covariates
  dist_dat <- lapply(1:length(l$vars), function(ii){
    return(ua_distr(name = l$vars[[ii]], data = l$data,
                    lims = l$lims[[ii]], type = l$type[[ii]],
                    resp = l$resp, n_dens = n_dens))
  })

  names(dist_dat) <- l$vars

  # Step 2. Fit a model to the training data
  # We've already fit the model, but we need the coefficient estimates and
  # variance-covariance matrix here.
  b <- coef(object$model)
  S <- stats::vcov(object$model)

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
    # Get habitat formula
    ff <- issf_w_form(object, l)

    # Drop movement variables from the betas
    bb <- bb[which(!names(bb) %in% l$move)]

    # Calculate w(x) under the model
    ww <- calc_w(f = ff, b = bb, newdata = l$data)

    # Attach to data
    l$data$w <- ww

    # Stratified sampling for iSSF -- one step per stratum
    dd <- do.call(rbind, lapply(unique(l$data[[l$strat]]), function(s) {
      # Subset to stratum
      strat <- l$data[which(l$data[[l$strat]] == s), ]

      # Normalize weights
      WW <- strat$w/sum(strat$w)

      # Sample
      rr <- sample.int(n = nrow(strat), size = 1, replace = TRUE,
                       prob = WW)

      # Return
      return(strat[rr, ])
    }))

    ### Step 3c. Summarize covariates at these locations
    # All of 'dd' should be treated as 'used' here
    dd[[l$resp]] <- TRUE
    cov_dens <- lapply(1:length(l$vars), function(ii){
      # Summarize distributions
      xx <- ua_distr(name = l$vars[[ii]], data = dd, lims = l$lims[[ii]],
                     type = l$type[[ii]], resp = l$resp, n_dens = n_dens,
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
             samp = pred_dist_cov,
             vars = l$vars,
             lims = l$lims,
             type = l$type,
             resp = l$resp)

  # Assign class
  class(ll) <- c("uhc_data", class(ll))

  # Return
  return(ll)
}

#' Prepares `test_dat` for `uhc_prep()`
#'
#' Internal function to check and format `test_dat`
#'
#' @param object `[glm, fit_logit, fit_clogit]` \cr A fitted RSF or (i)SSF model.
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
  f <- stats::formula(object)

  # Check that all predictor variables appear in 'test_dat'
  preds <- all.vars(stats::delete.response(stats::terms(f)))
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
  }

  # Types of each variable
  vars <- names(test_dat2)
  type <- ifelse(vars %in% fac, "factor", "numeric")
  names(type) <- vars

  # Calculate 'from' and 'to' for stats::density.default()
  #   Following the default for density() which calculates 'from' and 'to'
  #   as the range +/- bandwidth * 3. Only need to do this so that 'from'
  #   and 'to' are the same for both used and available.
  lims <- lapply(vars, function(vv) {
    if (type[vv] == "numeric") {
      # Range of data
      rng <- range(test_dat[[vv]], na.rm = TRUE)
      # Difference from the range
      rng_diff <- stats::bw.nrd0(test_dat[[vv]]) * 3
      from <- rng[1] - rng_diff
      to <- rng[2] + rng_diff
      return(c(from, to))
    } else {
      return(c(NA, NA))
    }
  })
  names(lims) <- vars

  # Construct list to return
  l <- list(data = test_dat,
            vars = vars,
            type = type,
            lims = lims,
            resp = resp)

  # Return
  return(l)

}

prep_test_dat.fit_clogit <- function(object, test_dat, verbose = TRUE) {

  # Check that 'test_dat' has no NAs
  na_rows <- nrow(test_dat) - nrow(na.omit(test_dat))
  if (na_rows > 0) {
    stop("'test_dat' contains ", na_rows, " row(s) with NAs. ",
         "Remove NAs before proceeding.")
  }

  ## Check that 'test_dat' is consistent with 'object' formula.
  # Extract formula
  f <- stats::formula(object$model)

  # Check that all predictor variables appear in 'test_dat'
  preds <- all.vars(stats::delete.response(stats::terms(f)))
  if (any(!(preds %in% names(test_dat)))){
    stop("All variables in fitted model must appear in 'test_dat'.")
  }

  # Get response variable
  resp <- as.character(as.list(f)[[2]])[3]

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

  # Get stratum variable
  pred_char <- as.character(as.list(f)[[3]])
  strat <- pred_char[grep("strata(", pred_char, fixed = TRUE)]
  strat <- gsub("strata(", "", strat, fixed = TRUE)
  strat <- gsub(")", "", strat, fixed = TRUE)

  # Remove from predictors
  preds2 <- preds[which(preds != strat)]

  # Check that stratum variable exists in test_dat
  if (is.null(test_dat[[strat]])) {
    stop("You must include the stratum variable in 'test_dat'.")
  }

  # Get movement variables
  # Note: risk that users will use different names.
  if (verbose) {
    message("Assuming step-length variables contain the string 'sl_' and ",
            "turn-angle variables contain the string 'ta_'.")
  }

  sl_vars <- preds[grep("sl_", preds, fixed = TRUE)]
  ta_vars <- preds[grep("ta_", preds, fixed = TRUE)]
  move_vars <- c(sl_vars, ta_vars)

  # Remove from predictors
  preds2 <- preds2[which(!preds2 %in% move_vars)]

  # Columns to delete (coordinates, time, response, stratum, movement)
  del <- c("x1_", "x2_", "y1_", "y2_", "t1_", "t2_", "dt_", "sl_", "ta_",
           resp, strat, move_vars)
  del <- unique(del)
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
  }

  # Types of each variable
  vars <- names(test_dat2)
  type <- ifelse(vars %in% fac, "factor", "numeric")
  names(type) <- vars



  # Calculate 'from' and 'to' for stats::density.default()
  #   Following the default for density() which calculates 'from' and 'to'
  #   as the range +/- bandwidth * 3. Only need to do this so that 'from'
  #   and 'to' are the same for both used and available.
  lims <- lapply(vars, function(vv) {
    if (type[vv] == "numeric") {
      # Range of data
      rng <- range(test_dat[[vv]], na.rm = TRUE)
      # Difference from the range
      rng_diff <- stats::bw.nrd0(test_dat[[vv]]) * 3
      from <- rng[1] - rng_diff
      to <- rng[2] + rng_diff
      return(c(from, to))
    } else {
      return(c(NA, NA))
    }
  })
  names(lims) <- vars

  # Construct list to return
  l <- list(data = test_dat,
            vars = vars,
            type = type,
            lims = lims,
            resp = resp,
            strat = strat,
            move = move_vars)

  # Return
  return(l)
}

#' Summarize distribution of used and available
#'
#' Internal function to summarize distribution of numeric or factor variables
#'
#' @param name `[character]` Name of the column to summarize.
#' @param type `[character]` Either `"numeric"` or `"factor"` as returned by
#' \code{\link{prep_test_dat}()}.
#' @param data `[data.frame]` The `data.frame` containing the columns and the
#' response variable.
#' @param lims `[numeric(2)]` A `numeric` vector of length 2 containing the
#' range for the density calculation for all variables where `type == "numeric"`
#' as returned by \code{\link{prep_test_dat}()}. Will be passed to
#' `stats::density.default()` arguments `from` and `to`.
#' @param resp `[character]` Name of the response variable.
#' @param n_dens `[numeric]` A `numeric` vector of length 1 giving the number of
#' equally spaced points at which density (used, available, and sampled) is
#' estimated. Passed to `stats::density.default()`, which indicates that `n`
#' should usually be specified as a power of 2.
#' @param avail `[logical]` Should distribution be calculated for the available
#' locations? Defaults to `TRUE`, but should be false when summarizing the
#' bootstrapped "used" samples.
#'
ua_distr <- function(name, type, data, lims, resp,
                     n_dens, avail = TRUE) {
  u <- na.omit(data[[name]][which(data[[resp]] == 1)])
  a <- na.omit(data[[name]][which(data[[resp]] == 0)])

  # If 'type' is numeric
  if (type == "numeric") {
    f_u <- stats::density(u, bw = "nrd0", n = n_dens,
                          from = lims[1], to = lims[2])
    f_u_df <- data.frame(x = f_u$x, y = f_u$y, dist = "U")
    if (avail) {
      f_a <- stats::density(a, bw = "nrd0", n = n_dens,
                            from = lims[1], to = lims[2])
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
  Terms <- stats::delete.response(stats::terms(f))

  # Remove intercept from betas
  # (no intercept in fit_clogit)
  if ("(Intercept)" %in% names(b)) {
    b <- b[which(names(b) != "(Intercept)")]
  }

  # Create data matrix
  X <- stats::model.matrix(Terms, data = newdata)

  # Drop intercept from data matrix
  X <- X[, -1, drop = FALSE]

  # Linear combination
  l <- as.vector(X %*% b)

  # Exponentiate
  w <- exp(l)

  # Return
  return(w)
}

#' Create habitat formula from iSSF
#'
#' Creates a formula without movement variables
#'
#' @param object `[fit_clogit]` Fitted iSSF.
#' @param l `[list]` List returned by `prep_test_dat.fit_clogit()`
#'
issf_w_form <- function(object, l) {
  # Get formula terms from object
  tt <- terms(stats::formula(object$model))

  # Pull out RHS and separate terms
  rhs <- unlist(strsplit(as.character(tt)[[3]], " + ", fixed = TRUE))

  # Get location of movement terms
  mm <- unique(unname(unlist(sapply(l$move, function(x) {
    return(grep(x, rhs))
  }))))

  # Get location of stratum
  ss <- grep("strata(", rhs, fixed = TRUE)

  # Combine
  drops <- c(mm, ss)

  # Keep only habitat terms
  hab <- stats::drop.terms(tt, drops, keep.response = TRUE)

  # Make formula
  ff <- formula(hab)

  # Return
  return(ff)
}

#' Plot UHC plots
#'
#' Plot an object of class `uhc_data`
#'
#' @param x `[uhc_data]` An object of class `uhc_data`, as returned
#' by the function \code{\link{prep_uhc}()}.
#' @param ... Included for consistency with generic
#' \code{\link{plot}()}. Currently ignored.
#'
#' @details Makes plots mimicking those in Fieberg et al. (2018), with the
#' bootstrapped distribution in gray, the observed distribution in black,
#' and the available distribution as a dashed red line.
#'
#' @author Brian J. Smith
#'
#' @seealso \code{\link{prep_uhc}()}, \code{\link{conf_envelope}()},
#' \code{\link{plot.uhc_envelopes}()}
#'
#' @export
plot.uhc_data <- function(x, ...) {
  # Check input
  if (!inherits(x, "uhc_data")) {
    stop("Object 'x' must be of class 'uhc_data'. See ?prep_uhc.")
  }

  ## Determine x-limits
  # Sampled x-lims
  samp_xlim <- lapply(x$samp, function(cov) {
    # Note: as.numeric() needed to handle factors as well as numeric variables
    min <- min(as.numeric(cov$x), na.rm = TRUE)
    max <- max(as.numeric(cov$x), na.rm = TRUE)
    return(c("min" = min, "max" = max))
  })

  # Original x-lims
  orig_xlim <- lapply(x$orig, function(cov) {
    # Note: as.numeric() needed to handle factors as well as numeric variables
    min <- min(as.numeric(cov$x), na.rm = TRUE)
    max <- max(as.numeric(cov$x), na.rm = TRUE)
    return(c("min" = min, "max" = max))
  })

  # Combine
  cov_xlim <- lapply(x$vars, function(cov) {
    min <- min(c(samp_xlim[[cov]][["min"]], orig_xlim[[cov]][["min"]]))
    max <- max(c(samp_xlim[[cov]][["max"]], orig_xlim[[cov]][["max"]]))
    return(c("min" = min, "max" = max))
  })
  names(cov_xlim) <- x$vars

  ## Determine y-limits
  # (Always set lower limit to 0)

  # Sampled y-max
  samp_ymax <- lapply(x$samp, function(cov) {
    max(cov$y, na.rm = TRUE)
  })

  # Original y-max
  orig_ymax <- lapply(x$orig, function(cov) {
    max(cov$y, na.rm = TRUE)
  })

  # Combine
  cov_ymax <- lapply(x$vars, function(cov) {
    max(c(samp_ymax[[cov]], orig_ymax[[cov]]))
  })
  names(cov_ymax) <- x$vars

  # Plot
  for (cov in x$vars) {
    # Is it numeric?
    if (x$type[[cov]] == "numeric") {
      # Subset the data
      S <- x$samp[[cov]]
      U <- x$orig[[cov]][which(x$orig[[cov]]$dist == "U"), ]
      A <- x$orig[[cov]][which(x$orig[[cov]]$dist == "A"), ]
      # Setup the plot
      plot(NA, xlim = cov_xlim[[cov]], ylim = c(0, cov_ymax[[cov]]),
           xlab = cov, ylab = "Probability Density", main = cov)
      # Draw bootstrapped distributions
      for (i in 1:max(S$iter)) {
        SS <- S[which(S$iter == i), ]
        lines(SS$x, SS$y, col = "gray80")
      }
      # Add used distribution
      lines(U$x, U$y, col = "black")
      # Add available distribution
      lines(A$x, A$y, col = "red", lty = 2)

    } else {
      # Must be factor

      # Subset the data
      # Sampled
      S <- x$samp[[cov]]
      S$x_num <- as.numeric(S$x)
      lev <- levels(S$x)
      unq <- sort(unique(S$x_num))
      # Original
      U <- x$orig[[cov]][which(x$orig[[cov]]$dist == "U"), ]
      A <- x$orig[[cov]][which(x$orig[[cov]]$dist == "A"), ]
      # Setup the plot
      plot(NA, xlim = cov_xlim[[cov]], ylim = c(0, cov_ymax[[cov]]),
           xlab = cov, ylab = "Probability Mass", main = cov,
           axes = FALSE)
      graphics::axis(2)
      graphics::axis(1, at = unq, labels = lev)
      graphics::box()
      # Draw bootstrapped proportions
      points(S$x_num, S$y, col = "gray80",
             pch = 16)
      # Add used proportion
      points(U$x, U$y, col = "black", pch = 16)
      # Add available proportion
      points(A$x, A$y, col = "red", pch = 1)
    }
  }

  return(invisible(NULL))
}

#' Coerce a `uhc_data` object to `data.frame`
#'
#' Coerces `uhc_data` from `list` to `data.frame`
#'
#' @param x `[uhc_data]` An object of class `uhc_data`, as returned
#' by the function \code{\link{prep_uhc}()}.
#' @param row.names Included for consistency with generic
#' \code{\link{as.data.frame}()}. Currently ignored.
#' @param optional Included for consistency with generic
#' \code{\link{as.data.frame}()}. Currently ignored.
#' @param ... Included for consistency with generic
#' \code{\link{as.data.frame}()}. Currently ignored.
#'
#' @details This coercion aims to keep all of the information contained in
#' the `uhc_data` `list` in the resulting `data.frame` representation. Factors
#' are converted to numeric, but the levels are retained in the column
#' `"label"`.
#'
#' @author Brian J. Smith
#'
#' @return Returns a `data.frame` with columns:
#' - `var`: The name of the variable
#' - `x`: The x-coordinate of the density plot (the value of `var`).
#' - `y`: The y-coordinate of the density plot (the probability density for
#' a numeric `var` and the proportion for a factor `var`).
#' - `dist`: The distribution represented. Either `"U"` for used, `"A"` for
#' available, or `"S"` for sampled.
#' - `iter`: The iteration number if `dist == "S"`.
#' - `label`: The label if `var` is a factor.
#'
#' @seealso \code{\link{prep_uhc}()}, \code{\link{conf_envelope}()}
#'
#' @export
as.data.frame.uhc_data <- function(x, row.names = NULL, optional = FALSE, ...) {
  # Check input
  if (!inherits(x, "uhc_data")) {
    stop("Object 'x' must be of class 'uhc_data'. See ?prep_uhc.")
  }

  # Grab factor levels from any factors
  if (any(x$type == "factor")) {
    fac <- x$vars[which(x$type == "factor")]
    levs <- lapply(fac, function(cov) {
      xx <- data.frame(label = levels(x$orig[[cov]]$x))
      xx$x <- seq(1, nrow(xx), by = 1)
      return(xx)
    })
    names(levs) <- fac
    lev_df <- dplyr::bind_rows(levs, .id = "var")
  } else {
    lev_df <- data.frame()
  }

  # Now treat all as numeric

  # Combine uhc_data$orig into data.frame
  orig <- dplyr::bind_rows(lapply(x$orig, function(cov) {
    cov$x <- as.numeric(cov$x)
    return(cov)
  }), .id = "var")
  orig$iter <- NA

  # Combine uhc_data$samp into data.frame
  samp <- dplyr::bind_rows(lapply(x$samp, function(cov) {
    cov$x <- as.numeric(cov$x)
    return(cov)
  }), .id = "var")
  samp$dist <- "S"

  # Combine 'orig' and 'samp'
  comb <- dplyr::bind_rows(orig, samp)

  # Join factor levels
  # (if factors exist)
  if(nrow(lev_df) > 0) {
    suppressMessages(comb <- dplyr::left_join(comb, lev_df))
  }

  # Class
  class(comb) <- c("uhc_data_frame", class(comb))

  # Return
  return(comb)
}

#' Create confidence envelopes from a `uhc_data_frame`
#'
#' Simplifies sampled distributions in a `uhc_data_frame` to confidence envelopes
#'
#' @param x `[uhc_data]` An object of class `uhc_data_frame`, as returned
#' by the function \code{\link{as.data.frame.uhc_data}()}.
#'
#' @details This can dramatically improve plotting time for UHC plots by
#' simplifying the many sampled lines down to the boundaries of a polygon.
#'
#' @author Brian J. Smith
#'
#' @return Returns a `data.frame` with columns:
#' - `var`: The name of the variable
#' - `x`: The x-coordinate of the density plot (the value of `var`).
#' - `label`: If `var` is a `factor`, the label for the value given by `x`.
#' - `U`: The y-coordinate of the density plot for the use distribution.
#' - `A`: The y-coordinate of the density plot for the availability distribution.
#' - `CI*_lwr`: The lower bound of the confidence envelope for the corresponding
#' confidence level.
#' - `CI*_upr`: The upper bound of the confidence envelope for the corresponding
#' confidence level.
#'
#' @seealso \code{\link{prep_uhc}()}, \code{\link{plot.uhc_envelopes}()}
#'
#' @export
conf_envelope <- function(x, levels = c(0.95, 1.00)) {
  ## Check inputs

  # x
  if (!inherits(x, "uhc_data_frame")) {
    stop("Object 'x' must be of class 'uhc_data_frame'. See ?as.data.frame.uhc_data.")
  }

  # levels
  checkmate::assert_numeric(levels, lower = 0, upper = 1)

  # Separate sampled distribution from used and available
  ua <- x[which(x$dist %in% c("U", "A")), ]
  s <- x[which(x$dist == "S"), ]

  # Get quantiles for each confidence limit
  qs <- lapply(levels, function(lv) {
    lwr <- (1 - lv)/2
    upr <- 1 - lwr
    return(c(lwr, upr))
  })

  # Name quantiles
  names(qs) <- paste0("CI", levels * 100)

  # Summarize samples at each quantile
  ss <- lapply(names(qs), function(qnm) {
    # Summarize
    suppressMessages({ # don't want dplyr::summarize() messages
      summ <- s |>
        dplyr::group_by(var, x, label) |>
        dplyr::summarize(lwr = stats::quantile(y, prob = qs[[qnm]][1]),
                         upr = stats::quantile(y, prob = qs[[qnm]][2])) |>
        dplyr::arrange(var, x) |>
        as.data.frame()
    })
    # Append CI name to lwr and upr
    names(summ)[4:5] <- paste0(qnm, "_", c("lwr", "upr"))

    # Return summarized quantiles
    return(summ)
  })

  # Construct final data.frame

  # Pivot 'ua' from long to wide data
  uaw <- ua |>
    dplyr::select(-iter) |>
    tidyr::pivot_wider(names_from = dist,
                       values_from = y) |>
    dplyr::arrange(var, x) |>
    as.data.frame()

  # Now join each element of 'ss'
  for (i in 1:length(ss)) {
    uaw <- dplyr::left_join(uaw, ss[[i]], by = c("var", "x", "label"))
  }

  # Set class
  class(uaw) <- c("uhc_envelopes", class(uaw))

  # Return
  return(uaw)
}

#' Plot simplified UHC plots
#'
#' Plot an object of class `uhc_envelopes`
#'
#' @param x `[uhc_envelopes]` An object of class `uhc_envelopes`, as returned
#' by the function \code{\link{conf_envelope}()}.
#' @param ... Included for consistency with generic
#' \code{\link{plot}()}. Currently ignored.
#'
#' @details Makes plots mimicking those in Fieberg et al. (2018), with the
#' bootstrapped distribution in gray, the observed distribution in black,
#' and the available distribution as a dashed red line. This differs from
#' \code{\link{plot.uhc_data}()} in that the bootstrapped distribution
#' (in gray) is drawn as a polygon rather than (many) lines, speeding up
#' plotting performance.
#'
#' @author Brian J. Smith
#'
#' @seealso \code{\link{prep_uhc}()}, \code{\link{conf_envelope}()},
#' \code{\link{plot.uhc_data}()}
#'
#' @export
plot.uhc_envelopes <- function(x, ...) {
  # Check input
  if (!inherits(x, "uhc_envelopes")) {
    stop("Object 'x' must be of class 'uhc_envelopes'. See ?conf_envelope.")
  }

  # Split data by variable
  xx <- split(x, x$var)

  # Plot each variable
  lapply(xx, function(XX) {
    # Determine existing confidence levels
    ci_nams <- names(XX)[grep("CI", names(XX))]
    ci_nums <- unique(gsub("CI", "",
                           sapply(strsplit(ci_nams, "_"), getElement, 1)))
    # Decide colors for each confidence level
    line_col <- gray.colors(n = length(ci_nums),
                            start = 0.3, end = 0.9,
                            alpha = 0.5)
    # Decide line widths (only applicable to factors)
    line_wid <- seq(2, 4, length.out = length(ci_nums))

    # Determine plot limits
    xlims <- range(XX$x)
    ylims <- c(0, max(XX[[ncol(XX)]]))

    # Determine if XX is numeric or factor
    fac <- all(!is.na(XX$label))

    if (isTRUE(fac)) {
      # Setup blank plot
      plot(NA, xlim = xlims, ylim = ylims,
           xlab = XX$var[1], ylab = "Probability Mass", main = XX$var[1],
           axes = FALSE)
      graphics::axis(2)
      graphics::axis(1, at = XX$x, labels = XX$label)
      graphics::box()

      # Add confidence envelopes
      for (i in length(ci_nums):1) {
        segments(x0 = XX$x,
                 x1 = XX$x,
                 y0 = XX[[paste0("CI", ci_nums[i], "_lwr")]],
                 y1 = XX[[paste0("CI", ci_nums[i], "_upr")]],
                 col = line_col[i],
                 lwd = line_wid[i])
      }

      # Add used and available
      points(x = XX$x, y = XX$U, col = "black", pch = 16)
      points(x = XX$x, y = XX$A, col = "red", pch = 1)

    } else {

      # Setup blank plot
      plot(NA, xlim = xlims, ylim = ylims,
           xlab = XX$var[1], ylab = "Probability Density", main = XX$var[1])

      # Add confidence envelopes
      for (i in length(ci_nums):1) {
        polygon(x = c(XX$x, rev(XX$x)),
                y = c(XX[[paste0("CI", ci_nums[i], "_lwr")]],
                      rev(XX[[paste0("CI", ci_nums[i], "_upr")]])),
                col = line_col[i])
      }

      # Add used and available
      lines(x = XX$x, y = XX$U, col = "black", lty = 1)
      lines(x = XX$x, y = XX$A, col = "red", lty = 2)
    }

    return(invisible(NULL))
  })

  # Invisibly return x
  return(invisible(x))
}

#' Subset a `uhc_data` object
#'
#' @param i `[numeric` or `character]` A numeric vector to subset variables
#' by position or a character vector to subset variables by name.
#' @export
`[.uhc_data` <- function(x, i) {

  # i can be either numeric or character
  if (is.numeric(i)) {
    xx <- list(orig = x$orig[i],
               samp = x$samp[i],
               vars = x$vars[i],
               lims = x$lims[i],
               type = x$type[i],
               resp = x$resp)
    class(xx) <- class(x)
    return(xx)
  }

  if (is.character(i)) {
    xx <- list(orig = x$orig[i],
               samp = x$samp[i],
               vars = i,
               lims = x$lims[i],
               type = x$type[i],
               resp = x$resp)
    class(xx) <- class(x)
    return(xx)
  }

  # If neither numeric or character
  stop("You may subset a 'uhc_data' object with a numeric or character vector.")
}

