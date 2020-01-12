## Deprecated 2020-01-11: Everything should now be handeld by `distributions.R`
##
## # step-lengths ------------------------------------------------------------
##
##
## #' Fit a statistical distribution to step lengths.
## #'
## #' @param .tbl `[track_xy,track_xyt]` \cr A track.
## #' @param x `[expression = sl_]` \cr The name of the column containing step lengths, usually `sl_`.
## #' @param distr `[character(1)]` \cr Name of the distribution, currently only `gamma`-distribution is supported.
## #' @param na.rm `[logical(1)]` \cr Should `NA` be removed?
## #' @template dots_none
## #' @name fit_sl_dist
## #'
## #' @export
## #' @rdname fit_sl_dist
## #' @examples
## #' data(deer)
## #' stps <- steps_by_burst(deer)
## #' fit_sl_dist(stps, sl_)
##
## fit_sl_dist <- function(.tbl, x = sl_, ...){
##   fit_sl_dist_base(.tbl[deparse(substitute(x))], ...)
## }
##
##
## #' @export
## #' @rdname fit_sl_dist
## fit_sl_dist_base <- function(x, na.rm = TRUE, distr = "gamma", ...) {
##   if (is.list(x)) x <- unlist(x, use.names = FALSE, recursive = TRUE)
##   if (na.rm) x <- x[!is.na(x)]
##
##   if (any(x == 0)) {
##     stop("0 length steps are not possible, consider adding a random error")
##   }
##
##   if (distr == "gamma") {
##     list(
##       name = distr,
##       fit = fitdistrplus::fitdist(x, distr, keepdata = FALSE, lower = 0)
##     )
##   } else if (distr == "unif") {
##     list(
##       name = distr,
##       fit = fitdistrplus::fitdist(x, distr, keepdata = FALSE, lower = 0)
##     )
##   } else if (distr == "exp") {
##     list(
##       name = distr,
##       fit = fitdistrplus::fitdist(x, distr, keepdata = FALSE)
##     )
##   }
##
## }
##
## #' Step lengths parameters
## #'
## #' Returns the parameter of the distribution (e.g., gamma) fitted to the distribution of step lengths.
## #' @param x An object that contains either a fitted conditional logistic regression, random steps or just a fitted distribution.
## #' @param alpha `[numeric(1)=0.05]{0-1}`\cr Alpha value for calculating 1-alpha confidence intervals.
## #' @export
## #' @name sl_params
## #' @template dots_none
## params <- function (x, ...) {
##   UseMethod("params", x)
## }
##
## #' @export
## #' @rdname params
## params.fit_clogit <- function (x, ...) {
##   sl_params(x$sl_$fit)
## }
##
## #' @export
## #' @rdname params
## params.random_steps <- function (x, ...) {
##   params(attributes(x)$sl_$fit)
## }
##
##
## #' @export
## #' @rdname params
## params.fitdist <- function(x, alpha = 0.05, ...) {
##   if (x$distname == "gamma") {
##     if ("scale" %in% names(x$estimate)) {
##       est <- c(shape = unname(x$estimate["shape"]),
##                scale = unname(x$estimate["scale"]))
##       se <- c(shape = unname(x$sd["shape"]),
##                scale = unname(x$sd["scale"]))
##     } else {
##       est <- c(shape = unname(x$estimate["shape"]),
##                scale = 1 / unname(x$estimate["rate"]))
##       se <- c(shape = unname(x$sd["shape"]),
##                scale = est[2]^2 * unname(x$sd["rate"])) # approximated SE
##     }
##     tv <- stats::qt(1 - (alpha / 2), x$n - 1)
##     ci_l <- est - tv * se
##     ci_h <- est + tv * se
##     cbind(est, se, lower_ci = ci_l, upper_ci = ci_h)
##   } else {
##     stop("Function only works for gamma distr at the moment.")
##   }
## }
##
## #' Step-length distribution
## #'
## #' Returns the name of the distribution (e.g., gamma) fitted to the distribution of step lengths.
## #' @param x An object that contains either a fitted conditional logistic regression, random steps or just a fitted distribution.
## #' @template dots_none
## #' @export
## #' @name sl_distr
## sl_distr <- function (x, ...) {
##   UseMethod("sl_distr", x)
## }
##
## #' @export
## #' @rdname sl_distr
## sl_distr.list <- function(x, ...) {
##   x$name
## }
##
## #' @export
## #' @rdname sl_distr
## sl_distr.fit_clogit <- function (x, ...) {
##   sl_distr(x$sl_)
## }
##
## #' @export
## #' @rdname sl_distr
## sl_distr.random_steps <- function (x, ...) {
##   sl_distr(attributes(x)$sl_)
## }
##
##
## # turning angles ----------------------------------------------------------
## #' Fit a statistical distribution to the turn angles of a track
## #'
## #' @param .tbl `[track_xy,track_xyt]` \cr A track.
## #' @param x `[expression]` \cr The name of the column containing turn angles, usually `ta_`.
## #' @param distr `[character(1)]{'vonmises', 'unif'}` \cr Name of the distribution, currently only `vonmises` and `uniform` distribution is supported.
## #' @param na.rm `[logical(1)]` \cr Should `NA` be removed?
## #'
## #' @template dots_none
## #' @name fit_ta_dist
##
## #' @export
## #' @rdname fit_ta_dist
## fit_ta_dist <- function(.tbl, x, ...){
##   fit_ta_dist_base(.tbl[deparse(substitute(x))], ...)
## }
##
##
## #' @export
## #' @rdname fit_ta_dist
## fit_ta_dist_base <- function(x, na.rm = TRUE, distr = "vonmises", ...) {
##   if (is.list(x)) x <- unlist(x, use.names = FALSE, recursive = TRUE)
##   if (na.rm) x <- x[!is.na(x)]
##
##   if (distr == "vonmises") {
##     x <- circular::as.circular(x, type = "angles", units = "radians", template = "none",
##                                modulo = "asis", zero = 0, rotation = "counter")
##     fit <- circular::mle.vonmises(x, ...)
##     list(
##       name = "vonmises",
##       params = fit$kappa,
##       fit = fit
##     )
##   } else if (distr == "unif") {
##     list(
##       name = "unif",
##       params = c(-pi, pi),
##       fit = c(-pi, pi)
##     )
##   } else {
##     stop("Requested distribution not implemented")
##   }
## }
##
##
##
## #' Turn angle parameters
## #'
## #' Returns the parameter of the distribution (e.g., von Mises) fitted to the distribution of turn angles.
## #' @param x `[fit_clogit(1),random_steps(1)]` \cr An object that contains either a fitted conditional logistic regression, random steps or just a fitted distribution.
## #' @export
## #' @template dots_none
## #' @name ta_params
## ta_params <- function (x, ...) {
##   UseMethod("ta_params", x)
## }
##
## #' @export
## #' @rdname ta_params
## ta_params.fit_clogit <- function (x, ...) {
##   x$ta_$params
## }
##
## #' @export
## #' @rdname sl_params
## ta_params.random_steps <- function (x, ...) {
##   attributes(x)$ta_$params
## }
##
## #' @export
## #' @rdname ta_params
## ta_kappa <- function (x, ...) {
##   if (ta_distr(x) == "vonmises") {
##     ta_params(x)
##   } else {
##     stop("kappa works only with von mises distr.")
##   }
## }
##
##
## #' Turn angle distribution
## #'
## #' Returns the name of the distribution (e.g., von Mises) fitted to the distribution of turn angles.
## #' @param x `[fit_clogit(1),random_steps(1)]` \cr An object that contains either a fitted conditional logistic regression, random steps or just a fitted distribution.
## #' @template dots_none
## #' @export
## #' @name ta_distr
## ta_distr <- function (x, ...) {
##   UseMethod("ta_distr", x)
## }
##
## #' @export
## #' @rdname ta_distr
## ta_distr.list <- function(x, ...) {
##   x$name
## }
##
## #' @export
## #' @rdname ta_distr
## ta_distr.fit_clogit <- function (x, ...) {
##   ta_distr(x$ta_)
## }
##
## #' @export
## #' @rdname ta_distr
## ta_distr.random_steps <- function (x, ...) {
##   ta_distr(attributes(x)$ta_)
## }
##
## #' Get `kappa`
## #'
## #' Convenience function to extract kappa and its SE.
## #'
## #' @param x `[list]` \cr Fitted turn angle distribution.
## #' @template dots_none
## #' @export
## get_kappa <- function (x, ...) {
##   UseMethod("get_kappa", x)
## }
##
## #' @export
## get_kappa.list <- function (x, ...) {
##   tibble::tibble(
##     kappa = x$fit$kappa,
##     se = x$fit$se.kappa
##   )
## }
##
