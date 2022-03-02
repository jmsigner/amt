#' Create UHC Plots for a Fitted Model
#'
#' Creates used-habitat calibration plots
#'
#' @param object `[fit_logit, fit_clogit]` \cr A fitted RSF or (i)SSF model.
#' @param test_dat `[data.frame]` \cr A `data.frame` with *testing* data from
#' which to sample test points.
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
#' #' \donttest{
#'
#' }
#' @export
prep_uhc <- function(object, test_dat) {
  #Check inputs
  if(!inherits(object, c("glm", "fit_logit", "fit_clogit"))){
    stop("'object' should be an object of class 'glm', 'fit_logit' or 'fit_clogit'.")
  }
  UseMethod("prep_uhc", object)
}

#' @rdname prep_uhc
#' @export
prep_uhc.glm <- function() {

}
