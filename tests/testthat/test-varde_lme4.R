test_that("works with one component", {
  fit <- lme4::lmer(
    formula = Petal.Width ~ 1 + (1 | Species),
    data = datasets::iris
  )
  res <- varde(fit)
  expect_equal(res$component, c("Species", "Residual"))
  expect_equal(res$variance, c(0.803296, 0.041882), tolerance = 1e-4)
  expect_equal(res$percent, c(0.95045, 0.04955), tolerance = 1e-4)
  expect_equal(res$method, c("lmer", "lmer"))
})
