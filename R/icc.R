
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

#' @inherit calc_icc.data.frame
#' @inheritParams calc_icc.data.frame
#' @export
calc_icc <- function(.data,
                     subject = "subject",
                     rater = "rater",
                     score = "score",
                     k = NULL,
                     method = ggdist::mode_qi,
                     ci = 0.95,
                     chains = 4,
                     cores = 4,
                     iter = 5000,
                     subject_label = "Subject",
                     rater_label = "Rater",
                     residual_label = "Residual",
                     ...) {
  UseMethod("calc_icc")
}

#' Calculate Inter-Rater ICC
#'
#' Calculate variance component and inter-rater intraclass correlation estimates
#' using a Bayesian generalizability study.
#'
#' @param .data A data frame containing at least the variables identified in
#'   `subject`, `rater`, and `score`.
#' @param subject A string indicating the column name in `.data` that contains
#'   an identifier for the subject or thing being scored in each row (e.g.,
#'   person, image, or document). (default = `"subject"`)
#' @param rater A string indicating the column name in `.data` that contains an
#'   identifier for the rater or thing providing the score in each row (e.g.,
#'   rater, judge, or instrument). (default = `"rater"`)
#' @param score A string indicating the column name in `.data` that contains the
#'   numerical score representing the rating of each row's subject from that
#'   same row's rater (e.g., score, rating, judgment, measurement). (default =
#'   `"score"`)
#' @param k Either `NULL` to set the number of raters you would like to estimate
#'   the reliability of to the total number of unique raters observed in `.data`
#'   or an integer specifying the number of raters you would like to estimate
#'   the reliability of (see details below). (default = `NULL`)
#' @param method A function (ideally from [ggdist::point_interval()]) that
#'   returns a data frame containing a point estimate (`y`) and the lower
#'   (`ymin`) and upper (`ymax`) bounds of an interval estimate. (default =
#'   [ggdist::mode_qi()])
#' @param ci A finite number between 0 and 1 that represents the width of the
#'   credible intervals to estimate (e.g., 0.95 = 95% CI). (default = `0.95`)
#' @param chains An integer representing the number of Markov chains to use in
#'   estimation. Forwarded on to [brms::brm()]. (default = `4`)
#' @param cores An integer representing the number of cores to use when
#'   executing the chains in parallel. Forwarded on to [brms::brm()]. (default =
#'   `4`)
#' @param iter An integer representing the total number of interations per chain
#'   (including warmup). Forwarded on to [brms::brm()]. (default = `5000`)
#' @param subject_label A string that controls what subjects are called in the
#'   returned output objects and any subsequent plots. (default = `"Subject"`)
#' @param rater_label A string that controls what raters are called in the
#'   returned output objects and any subsequent plots. (default = `"Rater"`)
#' @param residual_label A string that controls what residuals are called in the
#'   returned output objects and any subsequent plots. (default = `"Residual"`)
#' @param ... Further arguments passed to [brms::brm()].
#' @return A list object of class "varde_icc" that includes three main elements:
#' * `$iccs_summary`: A [tibble::tibble()] containing summary information about
#'   each ICC estimate.
#' * `$vars_summary`: A [tibble::tibble()] containing summary information about
#'   each variance estimate.
#' * `$ints_summary`: A [tibble::tibble()] containing summary information about
#'   each random intercept estimate.
#' * `$iccs_posterior`: A matrix where each row is a single posterior sample and
#'   each column is an ICC estimate.
#' * `$vars_posterior`: A matrix where each row is a single posterior sample and
#'   each column is a variance estimate.
#' * `$ints_posterior`: A matrix where each row is a single posterior sample and
#'   each column is a random intercept estimate.
#' * `$config`: A list containing the specified `method`, `ci`, and `k` values.
#' * `$model`: The brmsfit object created by [brms::brm()] containing the full
#'   results of the Bayesian generalizability study.
#' @method calc_icc data.frame
#' @export
calc_icc.data.frame <- function(.data,
                     subject = "subject",
                     rater = "rater",
                     score = "score",
                     k = NULL,
                     method = ggdist::mode_qi,
                     ci = 0.95,
                     chains = 4,
                     cores = 4,
                     iter = 5000,
                     subject_label = "Subject",
                     rater_label = "Rater",
                     residual_label = "Residual",
                     ...) {

  assertthat::assert_that(rlang::is_null(k) || rlang::is_integerish(k, n = 1))
  assertthat::assert_that(rlang::is_double(ci, n = 1, finite = TRUE),
                          ci > 0, ci < 1)
  assertthat::assert_that(rlang::is_integerish(chains, n = 1, finite = TRUE),
                          chains >= 1)

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
    # TODO: Check if an explicitly one-way model is necessary
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
  res <- varde(fit, ci = ci)

  if (twoway) {
    var_labels <- c(subject_label, rater_label, residual_label)
  } else {
    var_labels <- c(subject_label, residual_label)
  }

  colnames(res$vars_posterior) <- var_labels
  res$vars_summary$component <- var_labels

  # Extract posterior draws as vectors
  vs <-  as.vector(res$vars_posterior[, subject_label])
  if (twoway) {
    vr <- as.vector(res$vars_posterior[, rater_label])
  } else {
    vr <- rep(NA_real_, length(vs))
  }
  vsr <- as.vector(res$vars_posterior[, residual_label])

  # Calculate the harmonic mean of the number of raters per subject
  khat <- calc_khat(srm)

  # Calculate the proportion of non-overlap for raters and subjects
  q <- calc_q(srm)

  # Calculate posterior for each intraclass correlation coefficient
  iccs <- cbind(
    "ICC(A,1)" = vs / (vs + vr + vsr),
    "ICC(A,k)" = vs / (vs + (vr + vsr) / k),
    "ICC(A,khat)" = vs / (vs + (vr + vsr) / khat),
    "ICC(C,1)" = vs / (vs + vsr),
    "ICC(C,k)" = vs / (vs + vsr / k),
    "ICC(Q,khat)" = vs / (vs + q * vr + vsr / khat)
  )

  # Construct ICC output tibble
  iccs_estimates <- get_estimates(iccs, method = method, ci = ci)

  iccs_summary <-
    tibble(
      term = colnames(iccs),
      estimate = iccs_estimates$y,
      lower = iccs_estimates$ymin,
      upper = iccs_estimates$ymax,
      raters = c(1, k, khat, 1, k, khat),
      error = rep(c("Absolute", "Relative"), each = 3)
    )

  varde_icc(
    iccs_summary = iccs_summary,
    vars_summary = res$vars_summary,
    ints_summary = res$ints_summary,
    iccs_posterior = iccs,
    vars_posterior = res$vars_posterior,
    ints_posterior = res$ints_posterior,
    config = list(method = method, ci = ci, k = k),
    model = fit
  )

}
