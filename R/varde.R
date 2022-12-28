# S3 Methods

#' @method varde brmsfit
#' @export
varde.brmsfit <- function(model, ci = 0.95) {

  assertthat::assert_that(check_convergence(model) == TRUE)
  assertthat::assert_that(rlang::is_double(ci, n = 1), ci > 0, ci < 1)

  # Extract posterior draws for SD parameters
  sds <- brms::as_draws_matrix(
    model,
    variable = c("^sd_", "sigma"),
    regex = TRUE,
    inc_warmup = FALSE
  )

  # Convert SD parameters to variances
  vars <- sds^2

  # Rename variance parameters
  colnames(vars) <- gsub("sd_", "", colnames(vars))
  colnames(vars) <- gsub("__Intercept", "", colnames(vars))
  colnames(vars) <- gsub("sigma", "Residual", colnames(vars))

  # Calculate posterior estimates
  vars_est <- get_point_estimates(vars)
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
    regex = TRUE,
    inc_warmup = FALSE
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

  varde_res(
    vars_summary = vars_summary,
    vars_posterior = vars,
    ints_summary = ints_summary,
    ints_posterior = ints,
    model = model
  )
}

#' @method varde varde_icc
#' @export
varde.varde_icc <- function(x, ci = 0.95) {
  varde(x$model, ci = ci)
}

get_point_estimates <- function(m, .f = "mode") {
  apply(X = m, MARGIN = 2, FUN = bayestestR::map_estimate)
}

get_ci_lower <- function(m, ci = 0.95) {
  apply(X = m, MARGIN = 2, FUN = stats::quantile, probs = (1 - ci) / 2)
}

get_ci_upper <- function(m, ci = 0.95) {
  apply(X = m, MARGIN = 2, FUN = stats::quantile, probs = ci + (1 - ci) / 2)
}
