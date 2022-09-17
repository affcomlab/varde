test_that("works with one component", {
  fit_lmer <- lme4::lmer(
    formula = Petal.Width ~ 1 + (1 | Species),
    data = datasets::iris
  )
  res <- varde(fit_lmer, ci = 0.95)
  expect_equal(res$component, c("Species", "Residual"))
  expect_equal(res$variance, c(0.80329617, 0.04188163), tolerance = 1e-5)
  expect_equal(res$percent, c(0.95044637, 0.04955363), tolerance = 1e-5)
  expect_equal(res$lower, c(0.15032671, 0.03360314), tolerance = 1e-5)
  expect_equal(res$upper, c(4.67461165, 0.05311789), tolerance = 1e-5)
  expect_equal(res$method, c("lmer", "lmer"))
})
