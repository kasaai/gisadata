test_that("auto cat", {
  library(tidyverse)
  data_dir <- gisa_data_dir()

  auto_cat <- data_dir %>%
    path("Auto-Cat-Report") %>%
    gisa_process_auto_cat()

  expect_identical(
    names(auto_cat),
    c("Catastrophe Report - All classes Comprehensive, All Perils and Specified Perils - AB",
      "Catastrophe Report - All classes Comprehensive, All Perils and Specified Perils - NB",
      "Catastrophe Report - All classes Comprehensive, All Perils and Specified Perils - NL",
      "Catastrophe Report - All classes Comprehensive, All Perils and Specified Perils - NS",
      "Catastrophe Report - All classes Comprehensive, All Perils and Specified Perils - ON",
      "Catastrophe Report - All classes Comprehensive, All Perils and Specified Perils - PE"
    )
  )

  expect_identical(
    auto_cat %>% map_dbl(nrow) %>% unname(),
    rep(500, 6)
  )

  walk(auto_cat, ~ expect_setequal(
    names(.x),
    c("written_vehicles", "earned_vehicles", "written_premium", "earned_premium",
      "claim_count", "claim_count_original", "claim_count_ompp", "loss_amount",
      "expense_amount", "loss_and_expense_amount", "section_number",
      "valuation_year", "company_identification", "major_vehicle_class",
      "minor_vehicle_class", "region", "province", "statistical_territory",
      "deductible_amount", "limit_amount", "kind_of_loss_code", "accident_half_year",
      "accident_date_cat", "claim_count_cat", "loss_amount_cat", "expense_amount_cat",
      "loss_and_expense_amount_cat", "factor_flag", "major_coverage_type",
      "minor_coverage_type", "loss_transfer_flag"))
    )

  minor_coverage_types <- gisadata:::minor_coverage_type_mapping() %>%
    pull(minor_coverage_type_mapped)

  walk(auto_cat, function(df) {
    expect_true(all(df$minor_coverage_type %in% minor_coverage_types))
  })

  major_coverage_types <- gisadata:::major_coverage_type_mapping() %>%
    pull(major_coverage_type_mapped)

  walk(auto_cat, function(df) {
    expect_true(all(df$major_coverage_type %in% major_coverage_types))
  })

  walk(auto_cat, function(df) {
    expect_true(all(df$factor_flag %in% c("Yes", "No")))
  })
})
