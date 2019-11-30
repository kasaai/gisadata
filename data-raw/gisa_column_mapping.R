gisa_column_mapping <- readr::read_csv(
  "data-raw/gisa_column_mapping.csv",
  col_types = "cc"
) %>%
  dplyr::distinct()

usethis::use_data(
  gisa_column_mapping,
  overwrite = TRUE, internal = TRUE
)
