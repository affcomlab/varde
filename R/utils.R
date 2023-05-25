#' @export
check_convergence <- function(model) {
  UseMethod("check_convergence")
}

#' @method check_convergence brmsfit
#' @export
check_convergence.brmsfit <- function(model) {
  !any(abs(brms::rhat(model) - 1) > .1)
}

#' @method check_convergence lmerMod
#' @export
check_convergence.lmerMod <- function(model) {
  warn <- model@optinfo$conv$lme4$messages
  is.null(warn) || !grepl('failed to converge', warn)
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

get_estimates <- function(m, method = ggdist::mode_qi, ci = 0.95) {
  apply(X = m, MARGIN = 2, FUN = method, .width = ci) |>
    dplyr::bind_rows(.id = "term")
}

bname <- function(x) {
  gsub(pattern = "_", replacement = "", x)
}

get_lmer_ints <- function(m) {
  l <- stats::coef(m)
  l2 <- lapply(l, \(x) datawizard::rownames_as_column(x, var = "id"))
  d <- Reduce(rbind, l2)
  out <- data.frame(
    component = rep(names(l), each = nrow(d) / length(l)),
    term = rep("Intercept", times = nrow(d)),
    id = d$id,
    estimate = d$`(Intercept)`,
    lower = NA_real_,
    upper = NA_real_
  )
  out
}
