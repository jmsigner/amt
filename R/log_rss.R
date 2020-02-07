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
#' @references
#'
#' Avgar, T., Lele, S.R., Keim, J.L., & Boyce, M.S.. (2017). Relative Selection
#' Strength: Quantifying effect size in habitat‐ and step‐selection inference.
#' \emph{Ecology and Evolution}, 7, 5322–5330.
#'
#' @examples
#'
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
#' @export
log_rss <- function(object, x1, x2){
  #Check inputs
  if(!inherits(object, c("fit_logit", "fit_clogit"))){
    stop("'object' should be an object of class 'fit_logit' or 'fit_clogit'.")
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

  #Calculate y_x
  y_x1 <- predict(object$model, newdata = x1, type = "link")
  y_x2 <- predict(object$model, newdata = x2, type = "link")

  #Include values of x1 in return data.frame
  df <- x1
  names(df) <- paste0(names(x1), "_x1")
  #Calculate log_rss
  df$log_rss <- unname(y_x1 - y_x2)

  #Compose the list to return
  res <- list(df = df,
              x1 = x1,
              x2 = x2)

  #Set the S3 class (could be used for plotting method later)
  class(res) <- c("log_rss", class(res))

  #Return log_rss
  return(res)
}
