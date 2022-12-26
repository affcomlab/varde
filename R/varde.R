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

  # Calculate posterior estimates
  vars_est <- get_point_estimates(vars, point_estimate)
  vars_clo <- get_ci_lower(vars, ci)
  vars_chi <- get_ci_upper(vars, ci)

  # Construct variances summary tibble
  vars_summary <-
    tibble(
      component = colnames(vars),
      term = "Variance",
      estimate = vars_est,
      lower = vars_clo,
      upper = vars_chi,
      percent = vars_est / sum(vars_est)
    )

  # Extract posterior draws for random intercept parameters
  ints <- brms::as_draws_matrix(
    model,
    variable = "^r_",
    regex = TRUE
  )

  # Rename random intercept parameters
  colnames(ints) <- gsub("r\\_", "", colnames(ints))
  colnames(ints) <- gsub(",Intercept\\]", "", colnames(ints))
  colnames(ints) <- gsub("\\[", "\\_", colnames(ints))

  # Construct random intercepts summary tibble
  ints_summary <-
    tibble(
      component = colnames(ints),
      term = "Intercept",
      estimate = get_point_estimates(ints, point_estimate),
      lower = get_ci_lower(ints, ci),
      upper = get_ci_upper(ints, ci)
    ) |>
    tidyr::separate(col = component, into = c("component", "id"), sep = "_")

  varde_res(vars_summary = vars_summary, vars_posterior = vars,
            ints_summary = ints_summary, ints_posterior = ints)
}

#' @method varde varde_icc
#' @export
varde.varde_icc <- function(x, point_estimate = "mode", ci = 0.95) {
  varde(x$model, point_estimate = point_estimate, ci = ci)
}

get_point_estimates <- function(m, .f = "mode") {
  if (.f == "mode") {
    est <- apply(X = m, MARGIN = 2, FUN = post_mode)
  } else if (.f == "median") {
    est <- apply(X = m, MARGIN = 2, FUN = median)
  } else if (.f == "mean") {
    est <- apply(X = m, MARGIN = 2, FUN = mean)
  }
  est
}

get_ci_lower <- function(m, ci = 0.95) {
  apply(X = m, MARGIN = 2, FUN = stats::quantile, probs = (1 - ci) / 2)
}

get_ci_upper <- function(m, ci = 0.95) {
  apply(X = m, MARGIN = 2, FUN = stats::quantile, probs = ci + (1 - ci) / 2)
}
