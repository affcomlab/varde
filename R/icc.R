
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
                     scores = c("score1", "score2"),
                     k = NULL,
                     method = ggdist::mode_qi,
                     ci = 0.95,
                     chains = 4,
                     iter = 5000,
                     ...) {
  UseMethod("calc_icc")
}

#' Calculate Inter-Rater ICC
#'
#' Calculate variance component and inter-rater intraclass correlation estimates
#' using a Bayesian generalizability study.
#'
#' @param .data Either a data frame containing at least the variables identified
#'   in `subject`, `rater`, and `score` or a brmsfit object.
#' @param subject A string indicating the column name in `.data` that contains
#'   an identifier for the subject or thing being scored in each row (e.g.,
#'   person, image, or document). (default = `"subject"`)
#' @param rater A string indicating the column name in `.data` that contains an
#'   identifier for the rater or thing providing the score in each row (e.g.,
#'   rater, judge, or instrument). (default = `"rater"`)
#' @param scores A character vector indicating the column names in `.data` that
#'   contain the numerical scores representing the rating of each row's subject
#'   from that same row's rater (e.g., score, rating, judgment, measurement).
#'   (default = `c("score1", "score2")`)
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
                                scores = c("score1", "score2"),
                                k = NULL,
                                method = ggdist::mode_qi,
                                ci = 0.95,
                                chains = 4,
                                iter = 5000,
                                ...) {

  assertthat::assert_that(
    rlang::is_null(k) || rlang::is_integerish(k, n = 1)
  )
  assertthat::assert_that(
    rlang::is_double(ci, n = 1, finite = TRUE),
    ci > 0, ci < 1
  )
  assertthat::assert_that(
    rlang::is_integerish(chains, n = 1, finite = TRUE),
    chains >= 1
  )

  # How many score variables were provided?
  v <- length(scores)

  # Create logical subject-rater matrices
  srm <- lapply(
    X = scores,
    FUN = create_srm,
    .data = .data,
    subject = subject,
    rater = rater
  )
  names(srm) <- scores

  # Count the number of raters who scored each subject
  ks <- lapply(X = srm, FUN = rowSums)

  # Count the number of subjects scored by each rater
  nk <- lapply(X = srm, FUN = colSums)

  # Remove all subjects that had no raters
  keep <- lapply(ks, function(x) names(x[x > 0])) |> unlist() |> unique()
  .data <- .data[.data[[subject]] %in% keep, ]

  # Remove all raters that had no subjects
  keep <- lapply(nk, function(x) names(x[x > 0])) |> unlist() |> unique()
  .data <- .data[.data[[rater]] %in% keep, ]

  # If not specified, set k as the number of unique raters
  if (is.null(k)) {
    k <- length(unique(.data[[rater]]))
  }

  # Construct mixed-effects formula
  twoway <- is_twoway(.data, subject, rater)
  # TODO: Check that is_twoway() works correctly for mv models
  # TODO: Check that everything works when scores contain weird characters
  if (twoway && v == 1) {
    formula <- brms::bf(paste0(
      scores, ' ~ 1 + (1 | ', subject, ') + (1 | ', rater, ')'
    ))
  } else if (!twoway && v == 1) {
    formula <- brms::bf(paste0(
      scores, ' ~ 1 + (1 | ', subject, ')'
    ))
  } else if (twoway && v > 1) {
    formula <- brms::bf(paste0(
      'mvbind(', paste(scores, collapse = ", "), ') | mi() ~ ',
      '1 + (1 | ', subject, ') + (1 | ', rater, ')'
    )) + brms::set_rescor(FALSE)
  } else if (!twoway && v > 1) {
    formula <- brms::bf(paste0(
      'mvbind(', paste(scores, collapse = ", "), ') | mi() ~ ',
      '1 + (1 | ', subject, ')'
    )) + brms::set_rescor(FALSE)
  } else {
    stop("Error determining model type")
  }

  # Fit Bayesian mixed-effects model
  fit <- brms::brm(
    formula = formula,
    data = .data,
    chains = chains,
    iter = iter,
    init = "random",
    ...
  )

  # Extract posterior draws from model
  res <- varde(fit, ci = ci)

  # Extract posterior draws as matrices
  if (v > 1) {
    vs <-  res$vars_posterior[, paste(subject, bname(scores), sep = "__")]
    if (twoway) {
      vr <- res$vars_posterior[, paste(rater, bname(scores), sep = "__")]
    } else {
      vr <- rep(NA_real_, length(vs))
    }
    vsr <- res$vars_posterior[, paste("Residual", bname(scores), sep = "__")]
  } else {
    vs <-  res$vars_posterior[, subject]
    if (twoway) {
      vr <- res$vars_posterior[, rater]
    } else {
      vr <- rep(NA_real_, length(vs))
    }
    vsr <- res$vars_posterior[, "Residual"]
  }

  colnames(vs) <- scores
  colnames(vr) <- scores
  colnames(vsr) <- scores

  # Calculate the harmonic mean of the number of raters per subject
  khat <- lapply(srm, calc_khat)

  # Calculate the proportion of non-overlap for raters and subjects
  q <- lapply(srm, calc_q)

  # Make matrices for k, khat, and q
  kmat <- matrix(rep(k, times = v * nrow(vs)), ncol = v, byrow = TRUE)
  khatmat <- matrix(
    rep(unlist(khat), times = nrow(vs)),
    ncol = v,
    byrow = TRUE
  )
  qmat <- matrix(
    rep(unlist(q), times = nrow(vs)),
    ncol = v,
    byrow = TRUE
  )

  # Calculate posterior for each intraclass correlation coefficient
  iccs <- cbind(
    vs / (vs + vr + vsr),
    vs / (vs + (vr + vsr) / khatmat),
    vs / (vs + (vr + vsr) / kmat),
    vs / (vs + vsr),
    vs / (vs + qmat * vr + vsr / khatmat),
    vs / (vs + vsr / kmat)
  )
  icc_names <- c("ICC(A,1)", "ICC(A,khat)", "ICC(A,k)",
                 "ICC(C,1)", "ICC(Q,khat)", "ICC(C,k)")
  colnames(iccs) <- paste(
    rep(icc_names, each = v),
    colnames(iccs),
    sep = "__"
  )

  # Construct ICC output tibble
  iccs_estimates <- get_estimates(iccs, method = method, ci = ci)

  iccs_summary <-
    tibble(
      term = colnames(iccs),
      estimate = iccs_estimates$y,
      lower = iccs_estimates$ymin,
      upper = iccs_estimates$ymax,
      raters = rep(c(rep(1, v), unlist(khat), rep(k, v)), times = 2),
      error = rep(c("Absolute", "Relative"), each = v * 3)
    ) |>
    tidyr::separate(col = term, into = c("term", "score"), sep = "__") |>
    dplyr::relocate(score, .before = 1) |>
    dplyr::arrange(score, error, raters)

  if (v == 1) {
    colnames(iccs) <- icc_names
  }

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


#' @method calc_icc brmsfit
#' @export
calc_icc.brmsfit <- function(.data,
                             subject = "subject",
                             rater = "rater",
                             scores = c("score1", "score2"),
                             k = NULL,
                             method = ggdist::mode_qi,
                             ci = 0.95,
                             ...) {

  assertthat::assert_that(
    rlang::is_null(k) || rlang::is_integerish(k, n = 1)
  )
  assertthat::assert_that(
    rlang::is_double(ci, n = 1, finite = TRUE),
    ci > 0, ci < 1
  )

  # How many score variables were provided?
  v <- length(scores)

  # Extract data from model
  fit <- .data
  .data <- fit$data

  # Create logical subject-rater matrices
  srm <- lapply(
    X = scores,
    FUN = create_srm,
    .data = .data,
    subject = subject,
    rater = rater
  )
  names(srm) <- scores

  # Count the number of raters who scored each subject
  ks <- lapply(X = srm, FUN = rowSums)

  # Count the number of subjects scored by each rater
  nk <- lapply(X = srm, FUN = colSums)

  # Remove all subjects that had no raters
  keep <- lapply(ks, function(x) names(x[x > 0])) |> unlist() |> unique()
  .data <- .data[.data[[subject]] %in% keep, ]

  # Remove all raters that had no subjects
  keep <- lapply(nk, function(x) names(x[x > 0])) |> unlist() |> unique()
  .data <- .data[.data[[rater]] %in% keep, ]

  # If not specified, set k as the number of unique raters
  if (is.null(k)) {
    k <- length(unique(.data[[rater]]))
  }

  # Construct mixed-effects formula
  twoway <- is_twoway(.data, subject, rater)

  # Extract posterior draws from model
  res <- varde(fit, ci = ci)

  # Extract posterior draws as matrices
  if (v > 1) {
    vs <-  res$vars_posterior[, paste(subject, bname(scores), sep = "__")]
    if (twoway) {
      vr <- res$vars_posterior[, paste(rater, bname(scores), sep = "__")]
    } else {
      vr <- rep(NA_real_, length(vs))
    }
    vsr <- res$vars_posterior[, paste("Residual", bname(scores), sep = "__")]
  } else {
    vs <-  res$vars_posterior[, subject]
    if (twoway) {
      vr <- res$vars_posterior[, rater]
    } else {
      vr <- rep(NA_real_, length(vs))
    }
    vsr <- res$vars_posterior[, "Residual"]
  }

  colnames(vs) <- scores
  colnames(vr) <- scores
  colnames(vsr) <- scores

  # Calculate the harmonic mean of the number of raters per subject
  khat <- lapply(srm, calc_khat)

  # Calculate the proportion of non-overlap for raters and subjects
  q <- lapply(srm, calc_q)

  # Make matrices for k, khat, and q
  kmat <- matrix(rep(k, times = v * nrow(vs)), ncol = v, byrow = TRUE)
  khatmat <- matrix(
    rep(unlist(khat), times = nrow(vs)),
    ncol = v,
    byrow = TRUE
  )
  qmat <- matrix(
    rep(unlist(q), times = nrow(vs)),
    ncol = v,
    byrow = TRUE
  )

  # Calculate posterior for each intraclass correlation coefficient
  iccs <- cbind(
    vs / (vs + vr + vsr),
    vs / (vs + (vr + vsr) / khatmat),
    vs / (vs + (vr + vsr) / kmat),
    vs / (vs + vsr),
    vs / (vs + qmat * vr + vsr / khatmat),
    vs / (vs + vsr / kmat)
  )
  icc_names <- c("ICC(A,1)", "ICC(A,khat)", "ICC(A,k)",
                 "ICC(C,1)", "ICC(Q,khat)", "ICC(C,k)")
  colnames(iccs) <- paste(
    rep(icc_names, each = v),
    colnames(iccs),
    sep = "__"
  )

  # Construct ICC output tibble
  iccs_estimates <- get_estimates(iccs, method = method, ci = ci)

  iccs_summary <-
    tibble(
      term = colnames(iccs),
      estimate = iccs_estimates$y,
      lower = iccs_estimates$ymin,
      upper = iccs_estimates$ymax,
      raters = rep(c(rep(1, v), unlist(khat), rep(k, v)), times = 2),
      error = rep(c("Absolute", "Relative"), each = v * 3)
    ) |>
    tidyr::separate(col = term, into = c("term", "score"), sep = "__") |>
    dplyr::relocate(score, .before = 1) |>
    dplyr::arrange(score, error, raters)

  if (v == 1) {
    colnames(iccs) <- icc_names
  }

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
