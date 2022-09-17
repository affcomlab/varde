#' @export
check_convergence <- function(model) {
  UseMethod("check_convergence")
}

#' @method check_convergence lmerMod
#' @export
check_convergence.lmerMod <- function(model) {
  warn <- model@optinfo$conv$lme4$messages
  is.null(warn) || !grepl('failed to converge', warn)
}

#' @method check_convergence brmsfit
#' @export
check_convergence.brmsfit <- function(model) {
  !any(abs(brms::rhat(model) - 1) > .1)
}

post_mode <- function(x) {
  d <- stats::density(x)
  d$x[which.max(d$y)]
}
