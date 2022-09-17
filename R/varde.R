#' @export
varde <- function(model, ci = 0.95) {
  UseMethod("varde")
}

#' @method varde lmerMod
#' @export
varde.lmerMod <- function(model, ci = 0.95) {
  assertthat::assert_that(check_convergence(model) == TRUE)
  assertthat::assert_that(is.null(ci) || rlang::is_double(ci, n = 1))

  # Calculate point estimates
  vars_est <- c(as.double(summary(model)$varcor), summary(model)$sigma^2)

  # Calculate interval estimates (or skip)
  if (!is.null(ci)) {
    # TODO: Replace profile CIs with Monte Carlo CIs
    ci_theta <- suppressMessages(
      lme4::confint.merMod(model, parm = "theta_", level = ci)
    )
    lower <- as.double(ci_theta[, 1])^2
    upper <- as.double(ci_theta[, 2])^2
  } else {
    lower <- NA_real_
    upper <- NA_real_
  }

  # Construct output tibble
  out <-
    tibble(
      component = c(names(summary(model)$varcor), "Residual"),
      variance = vars_est,
      lower = lower,
      upper = upper,
      percent = vars_est / sum(vars_est),
      method = "lmer"
    )

  varde_res(out)
}

#' @method varde brmsfit
#' @export
varde.brmsfit <- function(model, ci = 0.95) {
  assertthat::assert_that(check_convergence(model) == TRUE)
  assertthat::assert_that(rlang::is_double(ci, n = 1))

  # Extract posterior draws for SD parameters
  sds <- brms::as_draws_matrix(
    model,
    variable = c("^sd_", "sigma"),
    regex = TRUE
  )

  # Convert SD parameters to variances
  vars <- sds^2

  # Rename variance parameters
  colnames(vars) <- gsub("sd_", "", colnames(vars))
  colnames(vars) <- gsub("__Intercept", "", colnames(vars))
  colnames(vars) <- gsub("sigma", "Residual", colnames(vars))

  # Calculate point estimates as maximum a posteriori (i.e., mode)
  vars_map <- apply(X = vars, MARGIN = 2, FUN = post_mode)

  # Caluclate interval estimate as posterior percentile
  vars_clo <- apply(X = vars, MARGIN = 2, FUN = stats::quantile,
                    probs = (1 - ci) / 2)
  vars_chi <- apply(X = vars, MARGIN = 2, FUN = stats::quantile,
                    probs = ci + (1 - ci) / 2)

  # Construct output tibble
  out <-
    tibble(
      component = colnames(vars),
      variance = vars_map,
      lower = vars_clo,
      upper = vars_chi,
      percent = vars_map / sum(vars_map),
      method = "brms"
    )

  varde_res(out)
}

#' @exportClass verde_res
new_varde_res <- function(x = tibble()) {
  stopifnot(tibble::is_tibble(x))
  structure(x, class = c("varde_res", class(x)))
}

varde_res <- function(x = tibble()) {
  new_varde_res(x)
}
