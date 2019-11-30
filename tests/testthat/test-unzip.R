test_that("multiplication works", {
  ls_1 <- dir_ls("gisa-data", type = "file", recurse = TRUE) %>%
    purrr::map_chr(path_file) %>%
    unname()

  zips_dir <- create_zip_structure()

  extract_dir <- dir_create(path(tempdir(), "extracted"))
  extract_dir <- gisa_unzip(zips_dir, extract_dir = extract_dir)

  ls_2 <- dir_ls(extract_dir) %>%
    purrr::map_chr(path_file) %>%
    unname()

  expect_identical(ls_1, ls_2)

  dir_delete(zips_dir)
  dir_delete(extract_dir)
})
