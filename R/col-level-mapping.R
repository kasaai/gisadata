map_levels <- function(data, mapping_table) {
  mt_names <- colnames(mapping_table)
  original_col <- mt_names[[1]]
  mapped_col <- mt_names[[2]]

  if (original_col %in% colnames(data)) {
    data %>%
      dplyr::left_join(mapping_table,
                       by = original_col) %>%
      dplyr::select(-.data[[original_col]]) %>%
      dplyr::rename(!!original_col := .data[[mapped_col]])
  } else {
    data
  }
}

liability_major_class_mapping <- function() {
  tibble::tribble(
    ~major_class, ~major_class_mapped,
    "B And P SERVICES", "Business and Professional Services",
    "C. E. and I.", "Construction, Erection and Installation Services",
    "EDUCATION", "Education Services",
    "FARMING", "Farming Services",
    "FISHING", "Fishing and Hunting",
    "GOVERNMENT", "Government Services",
    "HEALTH SERV.", "Health Services",
    "HOSPITALITY", "Hospitality Services",
    "LOGGING", "Logging Services",
    "MEMBER ORG.", "Member Organization Services",
    "MFG./PROC.", "Manufacturing/Processing Services",
    "MINING", "Mining Services",
    "REALTY", "Real Estate Property",
    "RECREATION", "Recreation Services",
    "RETAIL", "Retail Services",
    "TRANSPORT", "Transportation Services",
    "UTILITIES", "Utilities Services",
    "WAREHOUSING", "Warehousing Services",
    "WHOLESALE", "Wholesale Services"
  )
}

liability_coverage_policy_form_mapping <- function() {
  tibble::tribble(
    ~coverage_policy_form, ~coverage_policy_form_mapped,
    "10", "CGL - with product/completed operations",
    "20", "CGL - without product/completed operations",
    "30", "Tenants' Legal Liability",
    "40", "Excess Liability",
    "41", "Umbrella Liability",
    "42", "Auto Liability - Excess Liability",
    "43", "Auto Liability - Non-owned Automobile",
    "44", "Auto Liability - Contingent Lessors Liability",
    "50", "Wrap-up Liability",
    "61", "Professional Liability (other than D&O)",
    "62", "Directors' and Officers' Liability",
    "70", "Pollution Liability - Written as a separate policy (e.g. Environmental Impairment Liability)",
    "71", "Pollution Liability - Buy-back",
    "80", "Employers' Liabiliy (including Voluntary compensation"
  )
}

liability_kind_of_loss_mapping <- function() {
  tibble::tribble(
    ~kind_of_loss_code, ~kind_of_loss_code_mapped,
    "11", "Bodily Injury - Premises & Operations",
    "12", "Property Damage - Premises & Operations",
    "13", "Personal Injury - Premises & Operations",
    "21", "Bodily Injury - Products & Completed Operations",
    "22", "Property Damage - Products & Completed Operations",
    "23", "Personal Injury - Products & Completed Operations",
    "31", "Bodily Injury - Professional Services",
    "32", "Property Damage - Professional Services",
    "33", "Personal Injury - Professional Services",
    "34", "Financial Loss - Professional Services",
    "42", "Property Damage - Tenants' Legal Liability",
    "51", "Bodily Injury - Employers' Liability",
    "61", "Bodily Injury - Pollution Liability",
    "62", "Property Damage - Pollution Liability",
    "69", "Other - Pollution Liability"
  )
}

paid_outstanding_indicator_mapping <- function() {
  tibble::tribble(
    ~paid_outstanding_indicator, ~paid_outstanding_indicator_mapped,
    "1", "Paid",
    "2", "Outstanding"
  )
}

#' Map liability categorical variable levels
#'
#' @param data Data frame with tidy names.
gisa_liab_map_levels <- function(data) {
  data %>%
    map_levels(liability_major_class_mapping()) %>%
    map_levels(liability_coverage_policy_form_mapping()) %>%
    map_levels(liability_kind_of_loss_mapping()) %>%
    map_levels(paid_outstanding_indicator_mapping())
}

auto_factor_flag_mapping <- function() {
  tibble::tribble(
    ~factor_flag, ~factor_flag_mapped,
    "0", "No",
    "1", "Yes"
  )
}

fleet_flag_mapping <- function() {
  tibble::tribble(
    ~fleet_flag, ~flee_flag_mapped,
    "0", "No",
    "1", "Fleet rated",
    "2", "Synthetic fleet"
  )
}

major_coverage_type_mapping <- function() {
  tibble::tribble(
    ~major_coverage_type, ~major_coverage_type_mapped,
    "AB", "Accident Benefits",
    "AP", "All Perils",
    "CL", "Collision",
    "CM", "Comprehensive",
    "DDTD", "Death, Dismemberment, and Total Disability",
    "EELE", "Excess Economic Loss Endorsement",
    "Misc", "Miscenallneous Coverages",
    "SP", "Specified Perils",
    "TPL", "Third Party Liability",
    "UA", "Uninsured Automobile",
    "UM", "Underinsured Motorist"
  )
}

minor_coverage_type_mapping <- function() {
  tibble::tribble(
    ~minor_coverage_type, ~minor_coverage_type_mapped,
    "AB", "Accident Benefits",
    "AP", "All Perils",
    "CL", "Collision",
    "CM", "Comprehensive",
    "DC", "Direct Compensation",
    "DDTD", "Death, Dismemberment, and Total Disability",
    "EELE", "Excess Economic Loss Endorsement",
    "MED", "Medical",
    "MS", "Miscellaneous Coverages",
    "PD", "Property Damage",
    "SIC", "Single Interest Collision",
    "SIFT", "Single Interest Fire and Theft",
    "SP", "Specified Perils",
    "TPL", "Third Party Liability",
    "UA", "Uninsured Automobile",
    "UM", "Underinsured Motorist"
  )
}

loss_transfer_flag_mapping <- function() {
  tibble::tribble(
    ~loss_transfer_flag, ~loss_transfer_flag_mapped,
    "0", "No",
    "1", "Yes"
  )
}

#' Map auto categorical variable levels
#'
#' @param data Data frame with tidy names.
gisa_auto_map_levels <- function(data) {

  data %>%
    map_levels(auto_factor_flag_mapping()) %>%
    map_levels(fleet_flag_mapping()) %>%
    map_levels(major_coverage_type_mapping()) %>%
    map_levels(minor_coverage_type_mapping()) %>%
    map_levels(loss_transfer_flag_mapping()) %>%
    map_levels(paid_outstanding_indicator_mapping())
}


