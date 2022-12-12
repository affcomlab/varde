#' @export
check_convergence <- function(model) {
  UseMethod("check_convergence")
}

#' @method check_convergence merMod
#' @export
check_convergence.merMod <- function(model, tolerance = 0.001) {
  rlang::is_installed("Matrix")

  # Check Messages
  warn <- model@optinfo$conv$lme4$messages
  okwarn <- is.null(warn) || !grepl('failed to converge', warn)

  # Check Hessian
  relgrad <- with(model@optinfo$derivs, Matrix::solve(Hessian, gradient))
  okgrad <- max(abs(relgrad)) < tolerance

  okwarn && okgrad
}

#' @method check_convergence brmsfit
#' @export
check_convergence.brmsfit <- function(model) {
  !any(abs(brms::rhat(model) - 1) > .1)
}

# Calculate the mode of a continuous distribution
post_mode <- function(x) {
  d <- stats::density(x)
  d$x[which.max(d$y)]
}

is_balanced <- function(.data, subject, rater) {
  # How many raters scored each subject?
  ks <- rowSums(table(.data[[subject]], by = .data[[rater]]))

  # Were all subjects scored by the same number of raters?
  length(unique(ks)) == 1
}

is_complete <- function(.data, subject, rater) {
  # How many subjects did each rater score?
  nk <- rowSums(table(.data[[rater]], by = .data[[subject]]))

  # How many unique subjects were there?
  n <- length(unique(.data[[subject]]))

  # Did all raters score all subjects?
  all(nk == n)
}

is_twoway <- function(.data, subject, rater) {
  # How many subjects did each rater score?
  nk <- rowSums(table(.data[[rater]], by = .data[[subject]]))

  # Did all raters NOT score just one subject?
  !all(nk == 1)
}

get_terms <- function(.data, subject, rater, k, error) {
  # Is the design balanced or unbalanced?
  bal <- is_balanced(.data, subject, rater)

  # Is the design complete or incomplete?
  com <- is_complete(.data, subject, rater)

  # Is the design two-way/crossed or one-way/nested?
  two <- is_twoway(.data, subject, rater)

  if (two && error == "Relative" && k == 1 && com) {
    "ICC(C,1)"
  } else if (two && error == "Relative" && k == 1 && !com) {
    "ICC(Q,1)"
  } else if (two && error == "Relative" && k > 1 && com) {
    "ICC(C,k)"
  } else if (two && error == "Relative" && k > 1 && !com) {
    "ICC(Q,khat)"
  } else if (two && error == "Absolute" && k == 1) {
    "ICC(A,1)"
  } else if (two && error == "Absolute" && k > 1 && bal) {
    "ICC(A,k)"
  } else if (two && error == "Absolute" && k > 1 && !bal) {
    "ICC(A,khat)"
  } else if (!two && k == 1) {
    "ICC(1)"
  } else if (!two && k > 1 && bal) {
    "ICC(k)"
  } else if (!two && k > 1 && !bal) {
    "ICC(khat)"
  } else {
    NA_character_
  }
}
