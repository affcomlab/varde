
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
