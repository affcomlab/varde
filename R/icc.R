## One-way designs

# nested design, single rating
icc_1 <- function(vs, vrins) {
  vs / (vs + vrins)
}

# nested design, average ratings, balanced number of raters
icc_k <- function(vs, vrins, k) {
  vs / (vs + vrins / k)
}

# nested design, average ratings, unbalanced number of raters
icc_khat <- function(vs, vrins, khat) {
  vs / (vs + vrins / khat)
}

## Two-way designs

# crossed design, absolute error, single rating
icc_a_1 <- function(vs, vr, vsr) {
  vs / (vs + vr + vsr)
}

# crossed design, absolute error, average ratings, balanced number of raters
icc_a_k <- function(vs, vr, vsr, k) {
  vs / (vs + (vr + vsr) / k)
}

# crossed design, absolute error, average ratings, unbalanced number of raters
icc_a_khat <- function(vs, vr, vsr, khat) {
  vs / (vs + (vr + vsr) / khat)
}

# crossed design, relative error, single rating, complete data
icc_c_1 <- function(vs, vsr) {
  vs / (vs + vsr)
}

# crossed design, relative error, single rating, incomplete data
icc_q_1 <- function(vs, vr, vsr, q) {
  vs / (vs + (q * vr) + vsr)
}

# crossed design, relative error, average ratings, complete data
icc_c_k <- function(vs, vsr, k) {
  vs / (vs + vsr / k)
}

# crossed design, relative error, average ratings, incomplete data
icc_q_khat <- function(vs, vr, vsr, q, khat) {
  vs / (vs + (q * vr) + (vsr / khat))
}


## Helper Functions

create_srm <- function(.data,
                       subject = "subject",
                       rater = "rater",
                       score = "score") {

  # Remove missing and non-finite scores
  na_index <- is.na(.data[[score]]) | !is.finite(.data[[score]])
  .data <- .data[!na_index, ]

  # Check if each subject was scored by each rater
  table(.data[[subject]], .data[[rater]], useNA = "no") > 0

  #TODO: Assign varde_srm class
}

#' @method calc_khat varde_ks
#' @export
calc_khat.varde_ks <- function(ks) {
  # Remove any subjects not rated by any raters
  ks <- ks[ks != 0]

  # Calculate harmonic mean
  length(ks) / sum(1 / ks)
}

#' @method calc_khat varde_srm
#' @export
calc_khat.varde_srm <- function(srm) {
  ks <- rowSums(srm)
  #TODO: Assign varde_ks class
  calc_khat.varde_ks(ks)
}

calc_khat.data.frame <- function(.data,
                                 subject = "subject",
                                 rater = "rater",
                                 score = "score") {

  srm <- create_srm(.data, subject = subject, rater = rater, score = score)
  calc_khat.varde_srm(srm)

}

# khat is the harmonic mean of number of raters per subject
#' @export
calc_khat <- function(x, ...) {
  UseMethod("calc_khat")
}

# q is the proportion of nonoverlap across raters
# srm is a binary matrix indicating whether each subject (row) was rated by
# each rater (column)
#TODO: Create methods for calculating q from srm or .data
calc_q <- function(srm) {

  # How many raters per subject?
  ks <- rowSums(srm)
  #TODO: Assign varde_ks class

  # Remove any subjects not rated by anyone
  srm <- srm[ks != 0, ]
  ks <- ks[ks != 0]

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
  (1 / khat) - (total_overlap / (n * (n - 1)))
}
