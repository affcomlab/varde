library(brms)

df <- posneg
df[1:30,"Positive"] <- NA

corfit <- brm(
  formula = bf(mvbind(Positive, Negative) | mi() ~ 1 + (1 |s| Image) + (1 |r| Rater)) + set_rescor(TRUE),
  chains = 4,
  cores = 4,
  iter = 2500,
  init = "random",
  data = df,
  control = list(adapt_delta = 0.9999)
)

indfit <- brm(
  formula = bf(mvbind(Positive, Negative) | mi() ~ 1 + (1 | Image) + (1 | Rater)) + set_rescor(FALSE),
  chains = 4,
  cores = 4,
  iter = 2500,
  init = "random",
  data = df,
  control = list(adapt_delta = 0.9999)
)
