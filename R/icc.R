
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
#' * `$summary`: A [tibble::tibble()] containing summary information about each
#'   variance component and ICC estimate.
#' * `$posterior`: A matrix where each row is a single posterior sample and each
#'   column is either a variance component or ICC estimate.
#' * `$model`: The brmsfit object created by [brms::brm()] containing the full
#'   results of the Bayesian generalizability study.
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
    icc_names <- c(subject_label, rater_label, residual_label)
  } else {
    brms_names <- c(
      paste0("sd_", subject, "__Intercept"),
      "sigma"
    )
    icc_names <- c(subject_label, residual_label)
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
  vs <-  as.vector(vars[, subject_label])
  if (twoway) {
    vr <- as.vector(vars[, rater_label])
  } else {
    vr <- rep(NA_real_, length(vs))
  }
  vsr <- as.vector(vars[, residual_label])

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
        paste0(subject_label, " Variance"),
        paste0(rater_label, " Variance"),
        paste0(residual_label, " Variance"),
        "ICC(A,1)", "ICC(A,k)", "ICC(A,khat)",
        "ICC(C,1)", "ICC(C,k)", "ICC(Q,khat)"
      ),
      est = icc_est,
      lower = icc_eti[1, ],
      upper = icc_eti[2, ],
      raters = c(NA_integer_, NA_integer_, NA_integer_, 1, k, khat, 1, k, khat),
      error = rep(c(NA_character_, "Absolute", "Relative"), each = 3)
    )

  varde_icc(
    summary = summary_df,
    posterior = do.call(cbind, icc_post),
    model = fit
  )

}
