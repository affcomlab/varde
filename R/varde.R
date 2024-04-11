# S3 Methods

#' @method varde brmsfit
#' @export
varde.brmsfit <- function(model,
                          method = ggdist::mode_qi,
                          ci = 0.95,
                          convert = TRUE) {

  assertthat::assert_that(check_convergence(model) == TRUE)
  assertthat::assert_that(rlang::is_double(ci, n = 1), ci > 0, ci < 1)

  # Check if model is multivariate
  is_mv <- brms::is.mvbrmsformula(model$formula)

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
  colnames(vars) <- gsub("_Intercept", "", colnames(vars))
  colnames(vars) <- gsub("sigma_", "Residual__", colnames(vars))
  colnames(vars) <- gsub("sigma", "Residual", colnames(vars))

  # Calculate posterior estimates
  vars_estimates <- get_estimates(vars, method = method, ci = ci)

  # Construct variances summary tibble
  vars_summary <-
    data.frame(
      component = colnames(vars),
      term = "Variance",
      estimate = vars_estimates$y,
      lower = vars_estimates$ymin,
      upper = vars_estimates$ymax,
      percent = vars_estimates$y / sum(vars_estimates$y)
    )

  if (is_mv) {
    ## TODO: Replace with base R code
    vars_summary <-
      tidyr::separate(
        vars_summary,
        col = component,
        into = c("component", "score"),
        sep = "\\_\\_"
      ) |>
      dplyr::mutate(percent = estimate / sum(estimate), .by = score) |>
      dplyr::arrange(score, component) |>
      dplyr::relocate(score, .before = 1)
  }

  # Extract posterior draws for random intercept parameters
  ints <- brms::as_draws_matrix(
    model,
    variable = "^r_",
    regex = TRUE,
    inc_warmup = FALSE
  )

  # Rename random intercept parameters
  colnames(ints) <- gsub("^r\\_", "", colnames(ints))
  colnames(ints) <- gsub(",Intercept\\]", "", colnames(ints))
  colnames(ints) <- gsub("\\[", "\\_\\_", colnames(ints))

  # Construct random intercepts summary tibble
  ints_estimates <- get_estimates(ints, method = method, ci = ci)
  ints_summary <-
    data.frame(
      component = colnames(ints),
      term = "Intercept",
      estimate = ints_estimates$y,
      lower = ints_estimates$ymin,
      upper = ints_estimates$ymax
    )

  if (is_mv) {
    ints_summary <-
      ints_summary |>
      tidyr::separate(
        col = component,
        into = c("component", "score", "id"),
        sep = "\\_\\_",
        convert = convert
      ) |>
      dplyr::relocate(score, .before = 1) |>
      dplyr::arrange(score, component, id)
  } else {
    ints_summary <-
      ints_summary |>
      tidyr::separate(
        col = component,
        into = c("component", "id"),
        sep = "\\_\\_",
        convert = convert
      )
  }

  varde_res(
    vars_summary = vars_summary,
    vars_posterior = vars,
    ints_summary = ints_summary,
    ints_posterior = ints,
    config = list(method = method, ci = ci),
    model = model
  )
}

#' @method varde lmerMod
#' @export
varde.lmerMod <- function(model,
                          method = ggdist::mean_qi,
                          ci = 0.95) {
  # does that NULL arguments work if passed already?
  assertthat::assert_that(check_convergence(model) == TRUE)





 sigma_CI <- varde_CI(model, ci) #simulation results

  #reorder
 vc <- as.data.frame(lme4::VarCorr(model))
 vc_row <- vc$grp
 #sigma_CIs <- sigma_CI$Results[vc_row,]

 # Calculate simulation estimates
 vars_estimates <- get_estimates(sigma_CI$Samples, method = method, ci = ci)
 #reorder according to model
 v <- vars_estimates[match(vars_estimates$term,vc_row),]

 #TODO
 # use simulation's mean or mode? Curently, tenHove et al (2022)
 # uses estimate from model as point estimate
 # but this is not the simulation's mode.

  # Construct variances summary tibble
  vars_summary <-
    data.frame(
      component = v$term,
      term = "Variance",
      estimate = v$y,
      lower = v$ymin,
      upper = v$ymax,
      percent = v$y / sum(v$y)
      )

  #need the samples as matrices
  dat_sim <- data.matrix(sigma_CI$Samples)

  varde_res(
    vars_summary = vars_summary,
    vars_posterior = dat_sim,
    ints_summary = get_lmer_ints(model),
    ints_posterior = matrix(),
    config = list(method = method, ci = ci),
    model = model
  )
}

#' @method varde varde_icc_brms
#' @export
varde.varde_icc_brms <- function(x, ...) {
  varde(x$model, method = x$config$method, ci = x$config$ci)
}


#' @method varde varde_icc_lme
#' @export
varde.varde_icc_lme <- function(x, ...) {
  #reformat summaries since no need to rerun Monte Carlo simulations

   model <- x$model
   method <- x$config$method
   ci = x$config$ci

  vars_summary <- x$vars_summary

  varde_res(
    vars_summary = vars_summary,
    vars_posterior = x$vars_posterior,
    ints_summary = get_lmer_ints(model),
    ints_posterior = matrix(),
    config = list(method = method, ci = ci),
    model = model
  )


}
