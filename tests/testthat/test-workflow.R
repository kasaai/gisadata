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

test_that("read and rename", {
  col_specs <- gisa_col_specs()
  headers <- gisa_headers()

  data_liab <- path(gisa_data_dir(), "CLSP") %>%
    dir_ls() %>%
    map_dfr(read_csv,
      col_names = headers$liability,
      col_types = col_specs$liability
    ) %>%
    gisa_rename_cols()

  expect_identical(nrow(data_liab), 15L)
  expect_identical(
    names(data_liab),
    c(
      "written_premium", "earned_premium", "reported_claim_count",
      "generated_claim_count", "loss_amount", "expense_amount", "loss_and_expense_amount",
      "section_number", "company_identification", "region", "province",
      "valuation_year", "accident_year", "entry_year", "major_class",
      "coverage_policy_form", "industry_code", "policy_type", "kind_of_loss_code",
      "kind_of_loss_indicator", "paid_outstanding_indicator", "claim_location",
      "type_of_expense", "policy_limit_group", "size_of_loss_range_group"
    )
  )
})
