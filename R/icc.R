
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

# calc_icc() --------------------------------------------------------------

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
                     k = NULL,
                     ci = 0.95,
                     chains = 4,
                     cores = 4,
                     iter = 5000,
                     ...) {

  assertthat::assert_that(rlang::is_null(k) || rlang::is_integerish(k, n = 1))

  # Create logical subject-rater matrix
  srm <- create_srm(.data, subject, rater, score)

  # Count the number of raters who scored each subject
  ks <- rowSums(srm)

  # Count the number of subjects scored by each rater
  nk <- colSums(srm)

  # Remove all subjects that had no raters
  .data <- .data[.data[[subject]] %in% names(ks[ks > 0]), ]
  # TODO: Check whether we should remove ks == 1 as well as ks == 0

  # Remove all raters that had no subjects
  .data <- .data[.data[[rater]] %in% names(nk[nk > 0]), ]

  # Update subject-rater matrix
  srm <- create_srm(.data, subject, rater, score)

  # If not specified, set k as the number of unique raters
  if (is.null(k)) {
    k <- length(unique(.data[[rater]]))
  }

  # Construct mixed-effects formula
  twoway <- is_twoway(.data, subject, rater)
  if (twoway) {
    formula <- paste0(score, ' ~ 1 + (1 | ', subject, ') + (1 | ', rater, ')')
  } else {
    # TODO: Check if this is necessary
    formula <- paste0(score, ' ~ 1 + (1 | ', subject, ')')
  }

  # Fit Bayesian mixed-effects model
  fit <- brms::brm(
    formula = formula,
    data = .data,
    chains = chains,
    cores = cores,
    iter = iter,
    init = "random",
    ...
  )

  # Extract posterior draws from model
  if (twoway) {
    brms_names <- c(
      paste0("sd_", subject, "__Intercept"),
      paste0("sd_", rater, "__Intercept"),
      "sigma"
    )
    icc_names <- c("Subject", "Rater", "Residual")
  } else {
    brms_names <- c(
      paste0("sd_", subject, "__Intercept"),
      "sigma"
    )
    icc_names <- c("Subject", "Residual")
  }
  sds <- brms::as_draws_matrix(
    x = fit,
    variable = brms_names,
    inc_warmup = FALSE
  )
  colnames(sds) <- icc_names

  # Convert estimates to variances
  vars <- sds^2

  # Extract posterior draws as vectors
  vs <-  as.vector(vars[, "Subject"])
  if (twoway) {
    vr <- as.vector(vars[, "Rater"])
  } else {
    vr <- rep(NA_real_, length(vs))
  }
  vsr <- as.vector(vars[, "Residual"])

  # Calculate the harmonic mean of the number of raters per subject
  khat <- calc_khat(srm)

  # Calculate the proportion of non-overlap for raters and subjects
  q <- calc_q(srm)

  icc_post <- list(
    var_s = vs,
    var_r = vr,
    var_e = vsr,
    icc_a_1 = vs / (vs + vr + vsr),
    icc_a_k = vs / (vs + (vr + vsr) / k),
    icc_a_khat = vs / (vs + (vr + vsr) / khat),
    icc_c_1 = vs / (vs + vsr),
    icc_c_k = vs / (vs + vsr / k),
    icc_q_khat = vs / (vs + q * vr + vsr / khat)
  )

  # Calculate point estimates
  icc_est <- vapply(
    X = icc_post,
    FUN = post_mode,
    FUN.VALUE = double(1)
  )

  # Calculate equal tail intervals
  icc_eti <- vapply(
    X = icc_post,
    FUN = stats::quantile,
    FUN.VALUE = double(2),
    probs = c(
      (1 - ci) / 2,
      ci + (1 - ci) / 2
    )
  )

  # Construct output tibble
  summary_df <-
    tibble(
      term = c(
        "Subject Variance", "Rater Variance", "Residual Variance",
        "ICC(A,1)", "ICC(A,k)", "ICC(A,khat)",
        "ICC(C,1)", "ICC(C,k)", "ICC(Q,khat)"
      ),
      icc = icc_est,
      lower = icc_eti[1, ],
      upper = icc_eti[2, ],
      raters = c(NA_integer_, NA_integer_, NA_integer_, 1, k, khat, 1, k, khat),
      error = rep(c(NA_character_, "Absolute", "Relative"), each = 3)
    )

  varde_icc(summary = summary_df, posterior = do.call(cbind, icc_post))

}
