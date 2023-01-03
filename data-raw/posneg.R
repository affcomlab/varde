posneg <- readr::read_csv("./data-raw/posneg.csv", col_types = "iiii")

usethis::use_data(posneg, overwrite = TRUE)
