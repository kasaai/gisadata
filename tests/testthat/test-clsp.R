test_that("clsp", {
  data_dir <- gisa_data_dir()

  clsp <- data_dir %>%
    path("CLSP") %>%
    gisa_process_clsp()

  target_exhibits <- c(
    "Analysis by Large Incurred Losses by Coverage/Policy Form within Major Class - Ontario",
    "Analysis by Large Incurred Losses by Industry Code - Ontario",
    "Loss Development - Auto Liability - Contingent Lessors Liability - Ontario",
    "Loss Development - Auto Liability - Excess Liability - Ontario",
    "Loss Development - Auto Liability - Non-owned Automobile - Ontario",
    "Loss Development - CGL - with product/completed operations - Ontario",
    "Loss Development - CGL - without product/completed operations - Ontario",
    "Loss Development - Directors' & Officers' Liability - Claims-made basis - Ontario",
    "Loss Development - Directors' & Officers' Liability - Occurrence basis - Ontario",
    "Loss Development - Employers' Liability (including Voluntary compensation) - Ontario",
    "Loss Development - Excess Liability - Ontario", "Loss Development - Pollution Liability - Buy-back, Employers' Liability (including Voluntary compensation) - Ontario",
    "Loss Development - Pollution Liability - Written as a separate policy (e.g. Environmental Impairment Liability) - Ontario",
    "Loss Development - Professional Liability (other than Directors' & Officers' Liability) - Claims-made basis - Ontario",
    "Loss Development - Professional Liability (other than Directors' & Officers' Liability) - Occurrence basis - Ontario",
    "Loss Development - Tenants' Legal Liability - Ontario", "Loss Development - Umbrella Liability - Ontario",
    "Loss Development - Wrap-up Liability - Ontario")

  expect_setequal(
    names(clsp),
    target_exhibits
  )

  iwalk(clsp, function(df, exhibit) {
    if (grepl("Analysis by Large Incurred Losses by Coverage", exhibit)) {
      expect_setequal(
        names(df),
        c("generated_claim_count", "loss_amount", "expense_amount", "loss_and_expense_amount",
          "section_number", "company_identification", "region", "province",
          "valuation_year", "accident_year", "kind_of_loss_indicator",
          "size_of_loss_range_group", "major_class", "coverage_policy_form"
        )
      )
    } else if (grepl("Analysis by Large Incurred Losses by Industry Code", exhibit)) {
      expect_setequal(
        names(df),
        c("generated_claim_count", "loss_amount", "expense_amount", "loss_and_expense_amount",
          "section_number", "company_identification", "region", "province",
          "valuation_year", "accident_year", "industry_code", "size_of_loss_range_group"
        )
      )
    } else {
      expect_setequal(
        names(df),
        c("generated_claim_count", "loss_amount", "expense_amount", "loss_and_expense_amount",
          "section_number", "company_identification", "region", "province",
          "valuation_year", "accident_year", "entry_year", "policy_type",
          "kind_of_loss_indicator", "coverage_policy_form", "kind_of_loss_code",
          "paid_outstanding_indicator")
      )
    }
  })
})
