# S3 Methods

#' @method varde brmsfit
#' @export
varde.brmsfit <- function(model, method = ggdist::mode_qi, ci = 0.95) {

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
  vars_estimates <- get_estimates(vars, method = method, ci = ci)

  # Construct variances summary tibble
  vars_summary <-
    tibble(
      component = colnames(vars),
      term = "Variance",
      estimate = vars_estimates$y,
      lower = vars_estimates$ymin,
      upper = vars_estimates$ymax,
      percent = vars_estimates$y / sum(vars_estimates$y)
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
  ints_estimates <- get_estimates(ints, method = method, ci = ci)
  ints_summary <-
    tibble(
      component = colnames(ints),
      term = "Intercept",
      estimate = ints_estimates$y,
      lower = ints_estimates$ymin,
      upper = ints_estimates$ymax
    ) |>
    tidyr::separate(col = component, into = c("component", "id"), sep = "_")

  varde_res(
    vars_summary = vars_summary,
    vars_posterior = vars,
    ints_summary = ints_summary,
    ints_posterior = ints,
    config = list(method = method, ci = ci),
    model = model
  )
}

#' @method varde varde_icc
#' @export
varde.varde_icc <- function(x, ...) {
  varde(x$model, method = x$config$method, ci = x$config$ci)
}
