
# One-way ICCs ------------------------------------------------------------

## nested design, single rating
icc_1 <- function(vs, vrins) {
  vs / (vs + vrins)
}

## nested design, average ratings, balanced number of raters
icc_k <- function(vs, vrins, k) {
  vs / (vs + vrins / k)
}

## nested design, average ratings, unbalanced number of raters
icc_khat <- function(vs, vrins, khat) {
  vs / (vs + vrins / khat)
}

# Two-way ICCs ------------------------------------------------------------

## crossed design, absolute error, single rating
icc_a_1 <- function(vs, vr, vsr) {
  vs / (vs + vr + vsr)
}

## crossed design, absolute error, average ratings, balanced number of raters
icc_a_k <- function(vs, vr, vsr, k) {
  vs / (vs + (vr + vsr) / k)
}

## crossed design, absolute error, average ratings, unbalanced number of raters
icc_a_khat <- function(vs, vr, vsr, khat) {
  vs / (vs + (vr + vsr) / khat)
}

## crossed design, relative error, single rating, complete data
icc_c_1 <- function(vs, vsr) {
  vs / (vs + vsr)
}

## crossed design, relative error, single rating, incomplete data
icc_q_1 <- function(vs, vr, vsr, q) {
  vs / (vs + (q * vr) + vsr)
}

## crossed design, relative error, average ratings, complete data
icc_c_k <- function(vs, vsr, k) {
  vs / (vs + vsr / k)
}

## crossed design, relative error, average ratings, incomplete data
icc_q_khat <- function(vs, vr, vsr, q, khat) {
  vs / (vs + (q * vr) + (vsr / khat))
}

# Helper functions --------------------------------------------------------

create_srm <- function(.data,
                       subject = "subject",
                       rater = "rater",
                       score = "score") {

  #TODO: Allow subject, rater, and score to be specified using NSE
  assertthat::assert_that(is.data.frame(.data) || is.matrix(.data))
  cn <- colnames(.data)
  assertthat::assert_that(rlang::is_character(subject, n = 1), subject %in% cn)
  assertthat::assert_that(rlang::is_character(rater, n = 1), rater %in% cn)
  assertthat::assert_that(rlang::is_character(score, n = 1), score %in% cn)

  # Remove missing and non-finite scores
  na_index <- is.na(.data[[score]]) | !is.finite(.data[[score]])
  .data <- .data[!na_index, ]

  # Check if each subject was scored by each rater
  srm <- table(.data[[subject]], .data[[rater]], useNA = "no") > 0

  new_srm(srm)
}

#' @export
calc_icc <- function(x, ...) {
  UseMethod("calc_icc")
}

#' @method calc_icc data.frame
#' @export
calc_icc.data.frame <- function(.data,
                     subject = "subject",
                     rater = "rater",
                     score = "score",
                     method = c("lme4", "brms"),
                     unit = c("single", "average"),
                     ...) {

  method <- match.arg(method)
  unit <- match.arg(unit)

  # Remove all subjects that had no raters
  ks <- calc_ks(.data, subject = subject, rater = rater, score = score)
  .data <- .data[.data[[subject]] %in% names(ks[ks > 0]), ]
  # TODO: Check if we should remove ks == 1 as well as ks == 0

  # Construct mixed-effects formula
  formula <- paste0(score, ' ~ 1 + (1 | ', subject, ') + (1 | ', rater, ')')

  if (method == "lme4") {
    fit <- lme4::lmer(
      formula = formula,
      data = .data
    )
  } else if (method == "brms") {
    fit <- brms::brm(
      formula = formula,
      data = .data,
      chains = 4,
      cores = 4,
      init = "random",
      refresh = 0,
      silent = 2,
      ...
    )
  }

  res <- varde(fit)

  vs <- res[[which(res$component == subject), "variance"]]
  vr <- res[[which(res$component == rater), "variance"]]
  vsr <- res[[which(res$component == "Residual"), "variance"]]

  srm <- create_srm(.data, subject = subject, rater = rater, score = score)
  khat <- calc_khat(srm)
  q <- calc_q(srm)

  if (unit == "single") {
    ve_abs <- (vr + vsr)
    ve_rel <- (q * vr) + vsr
  } else if (unit == "average") {
    ve_abs <- (vr + vsr) / khat
    ve_rel <- ((q * vr) + vsr) / khat
  }

  icc_abs <- vs / (vs + ve_abs)
  icc_rel <- vs / (vs + ve_rel)

  out <- c(ICC_abs = icc_abs, ICC_rel = icc_rel)

  out

}

#' @method calc_icc varde_res
#' @export
calc_icc.varde_res <- function(res,
                               subject = "subject",
                               rater = "rater",
                               ...) {

  vs <- res[[which(res$component == subject), "variance"]]
  vr <- res[[which(res$component == rater), "variance"]]
  vsr <- res[[which(res$component == "Residual"), "variance"]]

  # TODO: Need to calculate khat and q from data and save into varde_res

}
