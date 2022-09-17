library(readr)
library(dplyr)

ppa_raw <- read_csv("./data-raw/Data.csv")

ppa <-
  ppa_raw |>
  select(
    Target = TargetNum,
    Rater = `Rater Code`,
    Type = StimulusType,
    Score = Rating
  ) |>
  mutate(
    Target = factor(Target),
    Rater = factor(Rater),
    across(everything(), as.integer)
  ) |>
  arrange(Target, Rater, Type) |>
  filter(Rater <= 72) |>
  print()

usethis::use_data(ppa, overwrite = TRUE)
