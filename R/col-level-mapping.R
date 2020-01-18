#' @importFrom rlang :=
map_levels <- function(data, mapping_table) {
  mt_names <- colnames(mapping_table)
  original_col <- mt_names[[1]]
  mapped_col <- mt_names[[2]]

  if (original_col %in% colnames(data)) {
    num_na <- data[[original_col]] %>%
      is.na() %>%
      sum()


    result <- data %>%
      dplyr::left_join(mapping_table,
                       by = original_col) %>%
      dplyr::select(-.data[[original_col]]) %>%
      dplyr::rename(!!original_col := .data[[mapped_col]])

    num_is_na_after_join <- data[[original_col]] %>%
      is.na() %>%
      sum()

    if (!identical(num_na, num_is_na_after_join)) {
      stop("Unmapped levels found.", call. = FALSE)
    }

    result
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
    "69", "Other - Pollution Liability",
    "70", "Auto Liability"
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
#' @keywords internal
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
    ~fleet_flag, ~fleet_flag_mapped,
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
    "UM", "Underinsured Motorist",
    # Not documented (https://github.com/kasaai/gisadata/issues/25)
    "ENTRUST", "Negligent Entrustment"
  )
}

minor_coverage_type_mapping <- function() {
  tibble::tribble(
    ~minor_coverage_type, ~minor_coverage_type_mapped,
    "AB", "Accident Benefits",
    "AP", "All Perils",
    # BI is undocumented, see #25
    "BI", "Bodily Injury",
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
    "UM", "Underinsured Motorist",
    # These are not documented (https://github.com/kasaai/gisadata/issues/25)
    "ENTRUST_CM", "Negligent Entrustment - Comprehensive",
    "ENTRUST_SP", "Negligent Entrustment - Specified Perils"
  )
}

loss_transfer_flag_mapping <- function() {
  tibble::tribble(
    ~loss_transfer_flag, ~loss_transfer_flag_mapped,
    "0", "No",
    "1", "Yes"
  )
}

auto_kind_of_loss_code_mapping <- function() {
  tibble::tribble(
    ~kind_of_loss_code, ~kind_of_loss_code_mapped,
    "01", "Bodily Injury by passengers in the insured automobile",
    "02", "Bodily Injury by any other third party",
    "03", "Bodily Injury, OUTSIDE PROVINCE, by passenger in the insured automobile",
    "04", "Bodily Injury, OUTSIDE PROVINCE, by any other third party.",
    "05", "Bodily Injury, WITHIN PROVINCE, by passengers in the insured automobile",
    "06", "Bodily Injury, WITHIN PROVINCE, by any other third party",
    "07", "Loss Transfer provision (applies to the company making the loss transfer only)",
    "09", "Property Damage",
    "12", "Property Damage to insured vehicle, constituting a total loss when OEF 43/ NBEF 43R or 19A is applicable",
    "14", "Damage to contents, not owned by the insured but under his care, custody or control",
    "15", "Property Damage to third party vehicle, or contents thereof, or to other property not under the care, custody or control of the insured",
    "16", "Other Property Damage to insured vehicle",
    "17", "Property Damage to contents owned by the insured",
    "18", "Loss of use",
    "19", "Damage to trailer, not owned by the insured, but under his care, custody or control",
    "20", "Collision claim",
    "21", "Fire claim",
    "22", "Theft Claim",
    "23", "Theft of the entire vehicle",
    "24", "Theft of contents of the vehicle",
    "25", "Malicious mischief and vandalism",
    "26", "Glass/windshield damage not caused by windstorm or hail",
    "27", "All other claims",
    "28", "Windstorm",
    "29", "Hail",
    "30", "Funeral services",
    "31", "Medical expenses",
    "32", "Death benefits",
    "34", "Disability income benefits",
    "35", "All Underinsured motorist claims",
    "36", "Uninsured Automobile - Bodily Injury",
    "38", "Uninsured Automobile - Property Damage",
    "37", "Accidents occurring outside Alberta or Ontario and payments in excess of provincial benefits",
    "39", "Uninsured and unidentified motorist benefits",
    "40", "Funeral benefits",
    "41", "Medical, excluding rehabilitation and extended care",
    "42", "Death benefits",
    "43", "Rehabilitation - renovations",
    "44", "Disability income benefits",
    "45", "Rehabilitation - other than renovations",
    "46", "Attendant care",
    "48", "Caregiver disability income benefits",
    "49", "Replacement of clothing, hearing aids, glasses and other devices",
    "60", "Loss Transfer recovery - funeral benefits",
    "61", "Loss Transfer recovery - medical, excluding rehabilitation and extended care",
    "62", "Loss Transfer recovery - death benefits",
    "63", "Loss Transfer recovery - rehabilitation renovations",
    "64", "Loss Transfer recovery - disability income benefits",
    "65", "Loss Transfer recovery - rehabilitation other than renovations",
    "66", "Loss Transfer recovery - Attendant care",
    "68", "Loss Transfer recovery - caregiver disability income benefits",
    "69", "Loss Transfer Recovery - replacement of clothing, hearing aids, glasses and other devices",
    "80", "Employed/deemed employed disability income benefits",
    "81", "Student disability income benefits",
    "82", "All others - non earner disability income benefits",
    "83", "Visitation expenses benefits",
    "84", "Dependant care expenses benefits",
    "85", "Housekeeping and home maintenance expenses benefits",
    "86", "Costs of examinations",
    "87", "All other including replacement of clothing, hearing aids, glasses and other devices",
    "90", "Loss Transfer recovery - employed/deemed employed disability income benefits",
    "91", "Loss Transfer recovery - student disability income benefits",
    "92", "Loss Transfer recovery - all others - non earner disability income benefits",
    "93", "Loss Transfer recovery - visitation expenses benefits",
    "94", "Loss Transfer recovery - dependant care expenses benefits",
    "95", "Loss Transfer recovery - housekeeping and home maintenance expenses benefits",
    "96", "Loss Transfer recovery - cost of examinations",
    "97", "Loss Transfer recovery - all other including replacement of clothing, hearing aids, glasses and other devices"
  )
}

#' Map auto categorical variable levels
#'
#' @param data Data frame with tidy names.
#' @keywords internal
gisa_auto_map_levels <- function(data) {

  data %>%
    map_levels(auto_factor_flag_mapping()) %>%
    map_levels(fleet_flag_mapping()) %>%
    map_levels(major_coverage_type_mapping()) %>%
    map_levels(minor_coverage_type_mapping()) %>%
    map_levels(loss_transfer_flag_mapping()) %>%
    map_levels(paid_outstanding_indicator_mapping()) %>%
    map_levels(auto_kind_of_loss_code_mapping())
}


