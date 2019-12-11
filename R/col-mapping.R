#' Rename columns
#'
#' @param data A data frame with GISA column names.
#' @export
gisa_rename_cols <- function(data) {
  cols_to_rename <- tibble::tibble(old_name = names(data)) %>%
    dplyr::inner_join(gisa_column_mapping, by = "old_name")

  if (nrow(cols_to_rename)) {
    renames <- cols_to_rename$old_name %>%
      stats::setNames(cols_to_rename$new_name)

    data %>%
      dplyr::rename(!!renames)
  } else {
    warning("No columns renamed by `gisa_rename_cols()`.", call. = FALSE)
    data
  }
}

#' Select columns based on format number
#'
#' @param data Data frame
#' @param format_number GISA format number
#' @export
gisa_select_cols <- function(data, format_number) {
  cols_to_select <- switch(
    format_number,
    "3" = c(
      "generated_claim_count",
      "loss_amount",
      "expense_amount",
      "loss_and_expense_amount",
      "section_number",
      "company_identification",
      "region",
      "province",
      "valuation_year",
      "accident_year",
      "major_class",
      "coverage_policy_form",
      "kind_of_loss_indicator",
      "size_of_loss_range_group"
    ),
    "4" = c(
      "generated_claim_count",
      "loss_amount",
      "expense_amount",
      "loss_and_expense_amount",
      "section_number",
      "company_identification",
      "region",
      "province",
      "valuation_year",
      "accident_year",
      "industry_code",
      "size_of_loss_range_group"
    ),
    "10" = c(
      "generated_claim_count",
      "loss_amount",
      "expense_amount",
      "loss_and_expense_amount",
      "section_number",
      "company_identification",
      "region",
      "province",
      "valuation_year",
      "accident_year",
      "entry_year",
      "coverage_policy_form",
      "policy_type",
      "kind_of_loss_code",
      "kind_of_loss_indicator",
      "paid_outstanding_indicator"
    ),
    "5" = c(
      "written_vehicles",
      "earned_vehicles",
      "written_premium",
      "earned_premium",
      "claim_count",
      "claim_count_original",
      "claim_count_ompp",
      "loss_amount",
      "expense_amount",
      "loss_and_expense_amount",
      "factor_flag",
      "section_number",
      "valuation_year",
      "company_identification",
      # Not currently used
      # "exhibit_type",
      "major_vehicle_class",
      "minor_vehicle_class",
      "trailer_indicator",
      "fleet_flag",
      "grid_indicator",
      "first_chance_indicator",
      "region",
      "province",
      "major_coverage_type",
      "minor_coverage_type",
      "deductible_amount",
      "limit_amount",
      "loss_transfer_flag",
      "accident_half_year"
    ),
    "6" = c(
      "written_vehicles",
      "earned_vehicles",
      "written_premium",
      "earned_premium",
      "claim_count",
      "claim_count_original",
      "claim_count_ompp",
      "loss_amount",
      "expense_amount",
      "loss_and_expense_amount",
      "factor_flag",
      "section_number",
      "valuation_year",
      "company_identification",
      # Not currently used
      # "exhibit_type",
      "major_vehicle_class",
      "minor_vehicle_class",
      "excluded_driver_code",
      "trailer_indicator",
      "fleet_flag",
      "grid_indicator",
      "first_chance_indicator",
      "region",
      "province",
      "major_coverage_type",
      "minor_coverage_type",
      "deductible_amount",
      "limit_amount",
      "kind_of_loss_code",
      "loss_transfer_flag",
      "paid_outstanding_indicator",
      "entry_half_year",
      "accident_half_year"
    )
  )

  data %>%
    dplyr::select(!!cols_to_select)
}
