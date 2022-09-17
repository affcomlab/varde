test_that("works for brms with one component", {
  testthat::skip_on_cran()

  fit_brms <- brms::brm(
    formula = Petal.Width ~ 1 + (1 | Species),
    data = datasets::iris,
    chains = 3,
    cores = 3,
    iter = 3000,
    init = "random",
    seed = 2022,
    adapt_delta = 0.99,
    max_treedepth = 15,
    backend = "cmdstanr",
    refresh = 0
  )
  res <- varde(fit_brms, ci = 0.95)
  testthat::expect_equal(res$component, c("Species", "Residual"))
  testthat::expect_equal(
    res$variance,
    c(Species = 0.8347026, Residual = 0.0416344),
    tolerance = 1e-5
  )
  testthat::expect_equal(
    res$lower,
    c(Species = 0.28298848, Residual = 0.03363698),
    tolerance = 1e-5
  )
  testthat::expect_equal(
    res$upper,
    c(Species = 18.45620518, Residual = 0.05414419),
    tolerance = 1e-5
  )
  testthat::expect_equal(
    res$percent,
    c(Species = 0.95249042, Residual = 0.04750958),
    tolerance = 1e-5
  )
  testthat::expect_equal(res$method, c("brms", "brms"))
})
