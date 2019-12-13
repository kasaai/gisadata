test_that("auto dev", {
  data_dir <- gisa_data_dir()

  auto_dev <- data_dir %>%
    path("Auto-Loss-Development") %>%
    gisa_process_auto_dev()

  target_exhibits <- c(
    "Loss development exhibit - Private Passenger - Accident Benefits - AB",
    "Loss development exhibit - Private Passenger - All Perils - AB",
    "Loss development exhibit - Private Passenger - Collision - AB",
    "Loss development exhibit - Private Passenger - Comprehensive - AB",
    "Loss development exhibit - Private Passenger - Exposures and Premium distribution - AB",
    "Loss development exhibit - Private Passenger - Specified Perils - AB",
    "Loss development exhibit - Private Passenger - Third Party Liability - AB",
    "Loss development exhibit - Private Passenger - Underinsured Motorist - AB",
    "Loss development exhibit - Private Passenger - Accident Benefits - NB",
    "Loss development exhibit - Private Passenger - Accident Benefits - NL",
    "Loss development exhibit - Private Passenger - Accident Benefits - NS",
    "Loss development exhibit - Private Passenger - Accident Benefits - PE",
    "Loss development exhibit - Private Passenger - All Perils - NB",
    "Loss development exhibit - Private Passenger - All Perils - NL",
    "Loss development exhibit - Private Passenger - All Perils - NS",
    "Loss development exhibit - Private Passenger - All Perils - PE",
    "Loss development exhibit - Private Passenger - Collision - NB",
    "Loss development exhibit - Private Passenger - Collision - NL",
    "Loss development exhibit - Private Passenger - Collision - NS",
    "Loss development exhibit - Private Passenger - Collision - PE",
    "Loss development exhibit - Private Passenger - Comprehensive - NB",
    "Loss development exhibit - Private Passenger - Comprehensive - NL",
    "Loss development exhibit - Private Passenger - Comprehensive - NS",
    "Loss development exhibit - Private Passenger - Comprehensive - PE",
    "Loss development exhibit - Private Passenger - Exposures and Premium distribution - NB",
    "Loss development exhibit - Private Passenger - Exposures and Premium distribution - NL",
    "Loss development exhibit - Private Passenger - Exposures and Premium distribution - NS",
    "Loss development exhibit - Private Passenger - Exposures and Premium distribution - PE",
    "Loss development exhibit - Private Passenger - Specified Perils - NB",
    "Loss development exhibit - Private Passenger - Specified Perils - NL",
    "Loss development exhibit - Private Passenger - Specified Perils - NS",
    "Loss development exhibit - Private Passenger - Specified Perils - PE",
    "Loss development exhibit - Private Passenger - Third Party Liability - NB",
    "Loss development exhibit - Private Passenger - Third Party Liability - NL",
    "Loss development exhibit - Private Passenger - Third Party Liability - NS",
    "Loss development exhibit - Private Passenger - Third Party Liability - PE",
    "Loss development exhibit - Private Passenger - Underinsured Motorist - NB",
    "Loss development exhibit - Private Passenger - Underinsured Motorist - NL",
    "Loss development exhibit - Private Passenger - Underinsured Motorist - NS",
    "Loss development exhibit - Private Passenger - Underinsured Motorist - PE",
    "Loss development exhibit - Private Passenger - Uninsured Automobile - NB",
    "Loss development exhibit - Private Passenger - Uninsured Automobile - NL",
    "Loss development exhibit - Private Passenger - Uninsured Automobile - NS",
    "Loss development exhibit - Private Passenger - Uninsured Automobile - PE",
    "Loss development exhibit - Private Passenger - Accident Benefits - ON",
    "Loss development exhibit - Private Passenger - All Perils - ON",
    "Loss development exhibit - Private Passenger - Collision - ON",
    "Loss development exhibit - Private Passenger - Comprehensive - ON",
    "Loss development exhibit - Private Passenger - Exposures and Premium distribution - ON",
    "Loss development exhibit - Private Passenger - Specified Perils - ON",
    "Loss development exhibit - Private Passenger - Third Party Liability - ON",
    "Loss development exhibit - Private Passenger - Underinsured Motorist - ON",
    "Loss development exhibit - Private Passenger - Uninsured Automobile - ON"
  )

  expect_setequal(
    names(auto_dev),
    target_exhibits
  )

  iwalk(auto_dev, function(df, exhibit) {
    if (grepl("Exposures and Premium distribution", exhibit)) {
      expect_setequal(
        names(df),
        c("written_vehicles", "earned_vehicles", "written_premium", "earned_premium",
          "claim_count", "claim_count_original", "claim_count_ompp", "loss_amount",
          "expense_amount", "loss_and_expense_amount", "section_number",
          "valuation_year", "company_identification", "major_vehicle_class",
          "minor_vehicle_class", "trailer_indicator", "grid_indicator",
          "first_chance_indicator", "region", "province", "deductible_amount",
          "limit_amount", "accident_half_year", "factor_flag", "fleet_flag",
          "major_coverage_type", "minor_coverage_type", "loss_transfer_flag"
        )
      )
    } else {
      expect_setequal(
        names(df),
        c("written_vehicles", "earned_vehicles", "written_premium", "earned_premium",
          "claim_count", "claim_count_original", "claim_count_ompp", "loss_amount",
          "expense_amount", "loss_and_expense_amount", "section_number",
          "valuation_year", "company_identification", "major_vehicle_class",
          "minor_vehicle_class", "excluded_driver_code", "trailer_indicator",
          "grid_indicator", "first_chance_indicator", "region", "province",
          "deductible_amount", "limit_amount", "kind_of_loss_code", "entry_half_year",
          "accident_half_year", "factor_flag", "fleet_flag", "major_coverage_type",
          "minor_coverage_type", "loss_transfer_flag", "paid_outstanding_indicator"
        )
      )
    }
  })

  minor_coverage_types <- gisadata:::minor_coverage_type_mapping() %>%
    pull(minor_coverage_type_mapped)

  walk(auto_dev, function(df) {
    expect_true(all(df$minor_coverage_type %in% minor_coverage_types))
  })

  major_coverage_types <- gisadata:::major_coverage_type_mapping() %>%
    pull(major_coverage_type_mapped)

  walk(auto_dev, function(df) {
    expect_true(all(df$major_coverage_type %in% major_coverage_types))
  })

  walk(auto_dev, function(df) {
    expect_true(all(df$factor_flag %in% c("Yes", "No")))
  })
})
