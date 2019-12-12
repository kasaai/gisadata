library(gisadata)
library(tidyverse)
library(fs)

extract_dir <- gisa_unzip("gisa-data", extract_dir = path(tempdir(), "gisa-data"))

c("Auto Cat Report", "Auto Loss Development", "CLSP") %>%
  walk(function(folder) {
    folder_path <- path(extract_dir, folder)
    folder_path %>%
      dir_ls() %>%
      walk(function(file_path) {
        file_path %>%
          read_csv(col_names = FALSE) %>%
          mutate_all(~ sample(.x, length(.x))) %>%
          sample_n(min(nrow(.), 500)) %>%
          write_csv(file_path, col_names = FALSE, na = "")
      })
    dir_copy(folder_path,
             path("inst/testdata/gisa-data", gsub(" ", "-", folder)),
             overwrite = TRUE)
  })
