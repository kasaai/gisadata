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

#' Map liability categorical variable levels
#'
#' @param data Data frame with tidy names.
#' @export
gisa_liab_map_levels <- function(data) {
  data %>%
    dplyr::left_join(liability_major_class_mapping(),
                     by = "major_class") %>%
    dplyr::select(-.data$major_class) %>%
    dplyr::rename(major_class = .data$major_class_mapped) %>%
    dplyr::left_join(liability_coverage_policy_form_mapping(),
                     by = "coverage_policy_form") %>%
    dplyr::select(-.data$coverage_policy_form) %>%
    dplyr::rename(coverage_policy_form = .data$coverage_policy_form_mapped) %>%
    dplyr::left_join(liability_kind_of_loss_mapping(),
                     by = "kind_of_loss_code") %>%
    dplyr::select(-.data$kind_of_loss_code) %>%
    dplyr::rename(kind_of_loss = .data$kind_of_loss_code_mapped)
}
