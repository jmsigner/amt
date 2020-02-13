#' Calculate log-RSS for a fitted model
#'
#' Calculate log-RSS(x1, x2) for a fitted RSF or (i)SSF
#'
#' @param object `[fit_logit, fit_clogit]` \cr A fitted RSF or (i)SSF model.
#' @param x1 `[data.frame]` \cr A `data.frame` representing the habitat values
#' at location x_1. Must contain all fitted covariates as expected by
#' `predict()`.
#' @param x2 `[data.frame]` \cr A 1-row `data.frame` representing the single
#' hypothetical location of x_2. Must contain all fitted covariates as expected
#' by `predict()`.
#'
#' @details This function assumes that the user would like to compare relative
#' selection strengths from at least one proposed location (`x1`) to exactly
#' one reference location (`x2`).
#'
#' The objects `object$model`, `x1`, and `x2` will be passed to
#' `predict()`. Therefore, the columns of `x1` and `x2` must match
#' the terms in the model formula exactly.
#'
#' @return Returns a `list` of class `log_rss`.
#'
#' @seealso See Avgar \emph{et al.}
#' (\href{https://doi.org/10.1002/ece3.3122}{2017}) for details about relative
#' selection strength.
#'
#' Default plotting method available: `\link{plot.log_rss}()`
#'
#' @references
#'
#' Avgar, T., Lele, S.R., Keim, J.L., & Boyce, M.S.. (2017). Relative Selection
#' Strength: Quantifying effect size in habitat‐ and step‐selection inference.
#' \emph{Ecology and Evolution}, 7, 5322–5330.
#'
#' @examples
#'
#' ############
#' ##Fit an RSF, then calculate log-RSS to visualize results.
#'
#' #Load packages
#' library(amt)
#' library(dplyr)
#' library(ggplot2)
#'
#' #Load data
#' data("amt_fisher")
#' data("amt_fisher_lu")
#'
#' #Prepare data for RSF
#' rsf_data <- amt_fisher %>%
#'   filter(burst_ == 1) %>%
#'   make_track(x_, y_, t_) %>%
#'   random_points() %>%
#'   extract_covariates(amt_fisher_lu) %>%
#'   mutate(landuse_study_area = factor(landuse_study_area)) %>%
#'   rename(lu = landuse_study_area)
#'
#' #Fit RSF
#' m1 <- rsf_data %>%
#'   fit_rsf(case_ ~ lu)
#'
#' #Calculate log-RSS
#' #data.frame of x1s
#' x1 <- data.frame(lu = sort(unique(rsf_data$lu)))
#' #data.frame of x2 (note factor levels should be same as model data)
#' x2 <- data.frame(lu = factor(21, levels = levels(rsf_data$lu)))
#' #Calculate
#' logRSS <- log_rss(object = m1, x1 = x1, x2 = x2)
#'
#' #Plot
#' ggplot(logRSS$df, aes(x = lu_x1, y = log_rss)) +
#'   geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
#'   geom_point() +
#'   xlab(expression("Land Use Category " * (x[1]))) +
#'   ylab("log-RSS") +
#'   ggtitle(expression("log-RSS" * (x[1] * ", " * x[2]))) +
#'   theme_bw()
#'
#' ##End RSF example
#' ############
#'
#' ############
#' ##Fit an SSF, then calculate log-RSS to visualize results.
#'
#' #Load packages
#' library(amt)
#' library(dplyr)
#' library(ggplot2)
#'
#' #Load data
#' data("deer")
#' data("sh_forest")
#'
#' #Prepare data for SSF
#' ssf_data <- deer %>%
#'   steps_by_burst() %>%
#'   random_steps(n = 15) %>%
#'   extract_covariates(sh_forest) %>%
#'   mutate(forest = factor(sh.forest, levels = 1:2,
#'                     labels = c("forest", "non-forest")),
#'   cos_ta = cos(ta_),
#'   log_sl = log(sl_))
#'
#' #Fit an SSF (note model = TRUE necessary for predict() to work)
#' m2 <- ssf_data %>%
#'   fit_clogit(case_ ~ forest + strata(step_id_), model = TRUE)
#'
#' #Calculate log-RSS
#' #data.frame of x1s
#' x1 <- data.frame(forest = factor(c("forest", "non-forest")))
#' #data.frame of x2
#' x2 <- data.frame(forest = factor("forest", levels = levels(ssf_data$forest)))
#' #Calculate
#' logRSS <- log_rss(object = m2, x1 = x1, x2 = x2)
#'
#' #Plot
#' ggplot(logRSS$df, aes(x = forest_x1, y = log_rss)) +
#'   geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
#'   geom_point(size = 3) +
#'   xlab(expression("Forest Cover " * (x[1]))) +
#'   ylab("log-RSS") +
#'   ggtitle(expression("log-RSS" * (x[1] * ", " * x[2]))) +
#'   theme_bw()
#'
#' ##End SSF example
#' ############
#'
#' @export
log_rss <- function(object, ...){
  #Check inputs
  if(!inherits(object, c("fit_logit", "fit_clogit"))){
    stop("'object' should be an object of class 'fit_logit' or 'fit_clogit'.")
  }
  UseMethod("log_rss", object)
}

#' @rdname log_rss
#' @export
log_rss.fit_logit <- function(object, x1, x2){
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

  #Calculate y_x
  y_x1 <- stats::predict.glm(object$model, newdata = x1, type = "link")
  y_x2 <- stats::predict.glm(object$model, newdata = x2, type = "link")

  #Include values of x1 in return data.frame
  df <- x1
  names(df) <- paste0(names(x1), "_x1")
  #Calculate log_rss
  df$log_rss <- unname(y_x1 - y_x2)

  #Compose the list to return
  res <- list(df = df,
              x1 = x1,
              x2 = x2,
              formula = object$model$formula)

  #Set the S3 class (could be used for plotting method later)
  class(res) <- c("log_rss", class(res))

  #Return log_rss
  return(res)
}

#' @rdname log_rss
#' @export
log_rss.fit_clogit <- function(object, x1, x2){
  if(is.null(object$model$model)){
    stop(paste("Please refit your step selection model with the argument 'model = TRUE'.\n"),
         "  The predict() method from package 'survival' requires that you store the model.\n",
         "  See the Examples under ?log_rss for a demonstration, and see Details under\n",
         "     ?survival:::predict.coxph for a full explanation.")
  }
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

  #Calculate correction due to sample-centered means (see ?survival:::predict.coxph for details)
  uncenter <- sum(coef(object$model) * object$model$means, na.rm=TRUE)
  #Calculate y_x
  y_x1 <- survival:::predict.coxph(object$model, newdata = x1, type = "lp", reference = "sample") + uncenter
  y_x2 <- survival:::predict.coxph(object$model, newdata = x2, type = "lp", reference = "sample") + uncenter

  #Include values of x1 in return data.frame
  df <- x1
  names(df) <- paste0(names(x1), "_x1")
  #Calculate log_rss
  df$log_rss <- unname(y_x1 - y_x2)

  #Compose the list to return
  res <- list(df = df,
              x1 = x1,
              x2 = x2,
              formula = object$model$formula)

  #Set the S3 class (could be used for plotting method later)
  class(res) <- c("log_rss", class(res))

  #Return log_rss
  return(res)
}


#' Plot a `log_rss` object
#'
#' Default plot method for an object of class `log_rss`
#'
#' @param object `[log_rss]` An object returned by the function `\link{log_rss}()`.
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
#' The function `\link{log_rss}()` is meant to accept a user-defined
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
#' change the default colors by specifying a custom `\link{palette}()` before
#' calling the function.
#'
#' @examples
#'
#' #Load packages
#' library(amt)
#'
#' #Load data
#' data("amt_fisher")
#' data("amt_fisher_lu")
#'
#' #Prepare data for RSF
#' rsf_data <- amt_fisher %>%
#'   filter(burst_ == 1) %>%
#'   make_track(x_, y_, t_) %>%
#'   random_points() %>%
#'   extract_covariates(amt_fisher_lu) %>%
#'   mutate(landuse_study_area = factor(landuse_study_area)) %>%
#'   rename(lu = landuse_study_area)
#'
#' #Fit RSF
#' m1 <- rsf_data %>%
#'   fit_rsf(case_ ~ lu)
#'
#' #Calculate log-RSS
#' #data.frame of x1s
#' x1 <- data.frame(lu = sort(unique(rsf_data$lu)))
#' #data.frame of x2 (note factor levels should be same as model data)
#' x2 <- data.frame(lu = factor(21, levels = levels(rsf_data$lu)))
#' #Calculate
#' logRSS <- log_rss(object = m1, x1 = x1, x2 = x2)
#'
#' ##Plot
#' plot(logRSS)
#'
#'
#' @export
plot.log_rss <- function(object, x_var1 = "guess", x_var2 = "guess", ...){
  #Check inputs
  if (!inherits(object, "log_rss")){
    stop("'object' must be of class 'log_rss'. See ?log_rss for details.")
  }
  checkmate::assert_character(x_var1)
  checkmate::assert_character(x_var2)

  #Guess x_vars if necessary
  if (x_var1 == "guess"){
    #Grab terms from the formula
    trms <- attr(terms(object$formula), "term.labels")
    #Grab x1 from the object
    x1_df <- object$x1
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

  #object$df has "_x1" appended to the names -- update x_vars to match
  x_var1_x1 <- amt:::append_x1(x_var1)
  x_var2_x1 <- amt:::append_x1(x_var2)

  #Decide whether plot should be lines or points
  cl_var1 <- class(object$df[[x_var1_x1]])
  if (cl_var1 == "numeric"){
    plot_type = "l"
  } else {
    plot_type = "p"
  }

  #Add color column
  if (is.na(x_var2)){
    #If there is no second x variable, color is constant
    object$df$color <- 1
  } else {
    #If there is, choose colors based on levels of x_var2
    object$df$color <- as.integer(factor(object$df[[x_var2_x1]]))
  }

  #Now plot
  if (plot_type == "p"){
    #If plotting points, we can plot them all at once
    graphics::plot(x = object$df[[x_var1_x1]], y = object$df$log_rss,
                   type = plot_type, col = object$df$color,
                   xlab = x_var1, ylab = "log-RSS",
                   main = expression("log-RSS(" * x[1] * " vs. " * x[2]*")"))
  } else {
    #If plotting lines, might have to separate by color
    if (max(object$df$color) > 1){
      #Setup plot
      graphics::plot(x = object$df[[x_var1_x1]], y = object$df$log_rss,
                     type = plot_type, col = par()$bg,
                     xlab = x_var1, ylab = "log-RSS",
                     main = expression("log-RSS(" * x[1] * " vs. " * x[2]*")"))
      for (i in 1:max(object$df$color)){
        df_sub <- object$df[which(object$df$color == i),]
        lines(x = df_sub[[x_var1_x1]], y = df_sub$log_rss, col = i)
      }
    } else {
      graphics::plot(x = object$df[[x_var1_x1]], y = object$df$log_rss,
                     type = plot_type, col = object$df$color,
                     xlab = x_var1, ylab = "log-RSS",
                     main = expression("log-RSS(" * x[1] * " vs. " * x[2]*")"))
    }
  }
}

#' Append "_x1"
#'
#' Helper function to append "_x1" to variable names
#'
#' @param string `[character]` Variable name to possibly append to
#'
#' @details The function first checks if "_x1" is already appended and adds it if
#' it is not. This is meant for internal use in `\link{plot.log_rss}()`.
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
    string <- paste0(string, "_x1")
    return(string)
  }
}
