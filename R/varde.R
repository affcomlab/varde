# S3 Methods

#' @method varde merMod
#' @export
varde.merMod <- function(model, ci = 0.95) {
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
varde.brmsfit <- function(model, point_estimate = "mode", ci = 0.95) {

  assertthat::assert_that(check_convergence(model) == TRUE)
  assertthat::assert_that(rlang::is_double(ci, n = 1))
  point_estimate = match.arg(point_estimate, c("mode", "median", "mean"))

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

  # Calculate point estimates
  if (point_estimate == "mode") {
    vars_est <- apply(X = vars, MARGIN = 2, FUN = post_mode)
  } else if (point_estimate == "median") {
    vars_est <- apply(X = vars, MARGIN = 2, FUN = median)
  } else if (point_estimate == "mean") {
    vars_est <- apply(X = vars, MARGIN = 2, FUN = mean)
  }

  # Calculate interval estimate as posterior percentile
  vars_clo <- apply(X = vars, MARGIN = 2, FUN = stats::quantile,
                    probs = (1 - ci) / 2)
  vars_chi <- apply(X = vars, MARGIN = 2, FUN = stats::quantile,
                    probs = ci + (1 - ci) / 2)

  # Construct output tibble
  summary_df <-
    tibble(
      component = colnames(vars),
      variance = vars_est,
      lower = vars_clo,
      upper = vars_chi,
      percent = vars_est / sum(vars_est),
      method = "brms"
    )

  varde_res(summary = summary_df, posterior = vars)
}

