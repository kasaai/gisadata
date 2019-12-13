test_that("clsp", {
  data_dir <- gisa_data_dir()

  clsp <- data_dir %>%
    path("CLSP") %>%
    gisa_process_clsp()

  target_exhibits <- c(
    "Analysis by Large Incurred Losses by Coverage/Policy Form within Major Class - Ontario",
    "Analysis by Large Incurred Losses by Industry Code - Ontario",
    "Auto Liability - Contingent Lessors Liability - Ontario", "Auto Liability - Excess Liability - Ontario",
    "Auto Liability - Non-owned Automobile - Ontario", "CGL - without product/completed operations - Ontario",
    "Directors' & Officers' Liability - Claims-made basis - Ontario",
    "Directors' & Officers' Liability - Occurrence basis - Ontario",
    "Employers' Liability (including Voluntary compensation) - Ontario",
    "Excess Liability - Ontario", "Loss Development - CGL - with product/completed operations - Ontario",
    "Pollution Liability - Buy-back, Employers' Liability (including Voluntary compensation) - Ontario",
    "Pollution Liability - Written as a separate policy (e.g. Environmental Impairment Liability) - Ontario",
    "Professional Liability (other than Directors' & Officers' Liability) - Claims-made basis - Ontario",
    "Professional Liability (other than Directors' & Officers' Liability) - Occurrence basis - Ontario",
    "Tenants' Legal Liability - Ontario", "Umbrella Liability - Ontario",
    "Wrap-up Liability - Ontario")

  expect_setequal(
    names(clsp),
    target_exhibits
  )
})
