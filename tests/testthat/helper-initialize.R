library(fs)
library(purrr)
library(readr)
library(dplyr)

gisa_data_dir <- function() {
  system.file("testdata", "gisa-data", package = "gisadata")
}

create_zip_structure <- function() {
  temp_dir <- dir_copy(gisa_data_dir(), tempdir())

  dir_ls(temp_dir, recurse = TRUE, regexp = "[.]CSV$") %>%
    walk(~ zip(
      zipfile = path(path_ext_set(.x, "zip")),
      .x,
      extras = "-q"
    ))

  dir_ls(temp_dir, regexp = "[.]CSV$", recurse = TRUE) %>%
    walk(file_delete)

  dir_ls(temp_dir, type = "directory") %>%
    walk(~ zip(
      zipfile = path(path_ext_set(.x, "zip")),
      .x,
      extras = "-q"
    ))

  dir_ls(temp_dir, type = "directory") %>%
    walk(dir_delete)

  temp_dir
}
