test_that("unzip works", {

  ls_1 <- dir_ls(gisa_data_dir(), type = "file", recurse = TRUE) %>%
    path_rel(gisa_data_dir())

  zips_dir <- create_zip_structure()

  extract_dir <- dir_create(path(tempdir(), "extracted"))
  extract_dir <- gisa_unzip(zips_dir, extract_dir = extract_dir)

  ls_2 <- dir_ls(extract_dir, type = "file", recurse = TRUE) %>%
    path_rel(extract_dir)

  expect_identical(ls_1, ls_2)
})
