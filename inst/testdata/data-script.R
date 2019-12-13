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
          mutate_at(vars(-one_of(c("X13", "X46"))), ~ sample(.x, length(.x))) %>%
          group_by(X13, X46) %>%
          sample_n(min(n(), 100)) %>%
          ungroup() %>%
          write_csv(file_path, col_names = FALSE, na = "")
      })
    dir_copy(folder_path,
             path("inst/testdata/gisa-data", gsub(" ", "-", folder)),
             overwrite = TRUE)
  })
