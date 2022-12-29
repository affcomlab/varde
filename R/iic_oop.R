# S3 Constructors ---------------------------------------------------------

new_ks <- function(x, ...) {
  structure(x, ..., class = "varde_ks")
}

new_srm <- function(x, ...) {
  structure(x, ..., class = "varde_srm")
}

new_icc <- function(iccs_summary = tibble::tibble(),
                    vars_summary = tibble::tibble(),
                    ints_summary = tibble::tibble(),
                    iccs_posterior = matrix(),
                    vars_posterior = matrix(),
                    ints_posterior = matrix(),
                    config = list(),
                    model = list()) {

  stopifnot(tibble::is_tibble(iccs_summary))
  stopifnot(tibble::is_tibble(vars_summary))
  stopifnot(tibble::is_tibble(ints_summary))
  stopifnot(is.matrix(iccs_posterior))
  stopifnot(is.matrix(vars_posterior))
  stopifnot(is.matrix(ints_posterior))
  stopifnot(is.list(config))
  stopifnot(inherits(model, "brmsfit"))

  structure(
    list(
      iccs_summary = iccs_summary,
      vars_summary = vars_summary,
      ints_summary = ints_summary,
      iccs_posterior = iccs_posterior,
      vars_posterior = vars_posterior,
      ints_posterior = ints_posterior,
      config = config,
      model = model
    ),
    class = "varde_icc"
  )
}

# S3 Helper
varde_icc <- function(iccs_summary = tibble::tibble(),
                      vars_summary = tibble::tibble(),
                      ints_summary = tibble::tibble(),
                      iccs_posterior = matrix(),
                      vars_posterior = matrix(),
                      ints_posterior = matrix(),
                      config = list(),
                      model = list()) {

  new_icc(
    iccs_summary,
    vars_summary,
    ints_summary,
    iccs_posterior,
    vars_posterior,
    ints_posterior,
    config,
    model
  )
}

# S3 Generics -------------------------------------------------------------

#' @export
calc_khat <- function(x, ...) {
  UseMethod("calc_khat")
}

#' @export
calc_ks <- function(x, ...) {
  UseMethod("calc_ks")
}

#' @export
calc_q <- function(x, ...) {
  UseMethod("calc_q")
}

# S3 Methods --------------------------------------------------------------

## calc_ks methods

#' @method calc_ks varde_srm
#' @export
calc_ks.varde_srm <- function(srm) {

  # Calculate ks from subject-by-rater matrix
  ks <- rowSums(srm)

  # Assign the varde_ks class
  ks <- new_ks(ks)

  # Return ks
  ks

}

#' @method calc_ks data.frame
#' @export
calc_ks.data.frame <- function(.data,
                               subject = "subject",
                               rater = "rater",
                               score = "score") {

  # Create subject-by-rater matrix from .data
  srm <- create_srm(.data, subject = subject, rater = rater, score = score)

  # Calculate ks from the subject-by-rater matrix
  ks <- calc_ks(srm)

  # Return ks
  ks

}

## calc_khat methods

#' @method calc_khat varde_ks
#' @export
calc_khat.varde_ks <- function(ks) {

  # Calculate harmonic mean
  khat <- length(ks) / sum(1 / ks)

  # Return khat
  khat

}

#' @method calc_khat varde_srm
#' @export
calc_khat.varde_srm <- function(srm) {

  # Count raters per subject from subject-by-rater matrix
  ks <- calc_ks(srm)

  # Calculate khat from raters per subject
  khat <- calc_khat(ks)

  # Return khat
  khat

}

#' @method calc_khat data.frame
#' @export
calc_khat.data.frame <- function(.data,
                                 subject = "subject",
                                 rater = "rater",
                                 score = "score",
                                 ...) {

  # Create subject-by-rater matrix from .data
  srm <- create_srm(.data, subject = subject, rater = rater, score = score, ...)

  # Calculate khat from subject-by-rater matrix
  khat <- calc_khat(srm)

  # Return khat
  khat

}

## calc_q methods

#' @method calc_q varde_srm
#' @export
calc_q.varde_srm <- function(srm) {

  # How many raters per subject?
  ks <- calc_ks(srm = srm)

  # How many subjects?
  n <- nrow(srm)

  # What is the harmonic mean of raters per subject?
  khat <- calc_khat(ks = ks)

  # Generate all unique pairs of subject indexes
  spairs <- combn(n, 2)

  # Function to calculate the proportion of overlap for a pair of subjects
  pair_overlap <- function(spair, srm) {
    # What is the index of first subject?
    s1 <- spair[[1]]

    # What is the index of second subject?
    s2 <- spair[[2]]

    # How many raters for first subject?
    k_s1 <- sum(srm[s1, ])

    # How many raters for second subject?
    k_s2 <- sum(srm[s2, ])

    # How many raters shared between subjects?
    k_s1s2 <- sum(colSums(srm[c(s1, s2), ]) > 1)

    # What is the proportion of rater overlap for this pair of subjects?
    (2 * k_s1s2) / (k_s1 * k_s2)

    # NOTE: Because we will iterate over unique pairs rather than all pairs...
    # NOTE: We double the numerator to capture both orderings (A-B and B-A)
    # NOTE: This saves a little time by halving the number of iterations needed
  }

  # Apply function to all unique pairs of subjects and sum across pairs
  total_overlap <- sum(apply(X = spairs, MARGIN = 2, FUN = pair_overlap, srm))

  # Calculate the proportion of non-overlap across subjects and raters
  q <- (1 / khat) - (total_overlap / (n * (n - 1)))

  # Return q
  q

}

#' @method calc_q data.frame
#' @export
calc_q.data.frame <- function(.data,
                              subject = "subject",
                              rater = "rater",
                              score = "score",
                              ...) {

  # Create subject-by-rater matrix from .data
  srm <- create_srm(.data, subject = subject, rater = rater, score = score, ...)

  # Calculate q from subject-by-rater matrix
  q <- calc_q(srm = srm)

  # Return q
  q

}

#' @export print.varde_icc
#' @export
print.varde_icc <- function(x, variances = TRUE, intercepts = TRUE, ...) {
  cat(crayon::blue("# ICC Estimates\n"))
  print(x$iccs_summary, ...)
  if (variances) {
    cat(crayon::blue("\n# Variance Estimates\n"))
    print(x$vars_summary, ...)
  }
  if (intercepts) {
    cat(crayon::blue("\n# Intercept Estimates\n"))
    print(x$ints_summary, ...)
  }
}

#' @export summary.varde_icc
#' @export
summary.varde_icc <- function(x,
                              which = "iccs",
                              ...) {

  match.arg(which, choices = c("iccs", "variances", "intercepts", "model"))
  if (which == "iccs") {
    out <- x$iccs_summary
  } else if (which == "variances") {
    out <- x$vars_summary
  } else if (which == "intercepts") {
    out <- x$ints_summary
  } else if (which == "model") {
    out <- summary(x$model, ...)
  }
  out
}
