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
            "glmmTMB::glmmTMB (in development)",
            "mgcv::gam (in development)"),
    ssf = c("survival::clogit",
            "glmmTMB::glmmTMB (in development)",
            "mgcv::gam (in development)")
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

# Return consistent model matrix ----
get_model_matrix <- function(object, ..., intercept = FALSE) {
  UseMethod("get_model_matrix", object)
}

#' @export
get_model_matrix.glm <- function(object, ..., intercept = FALSE) {
  mm <- stats::model.matrix(object, ...)
}
