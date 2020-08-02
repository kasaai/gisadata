library(gisadata)
library(fs)
library(tidyverse)

# Extract and load raw data

extract_dir <- gisa_unzip("gisa-data")
data_auto <- extract_dir %>%
  path("Auto Loss Development") %>%
  gisa_process_auto_dev()

dir_create("output")

# Define helper function

generate_template_data <- function(triangle_df,
                                   premiums_exposures_df,
                                   output_path) {
  count_triangle <- triangle_df %>%
    gisa_extract_triangle(type = "count")

  paid_triangle <- triangle_df %>%
    gisa_extract_triangle(type = "paid")

  incurred_triangle <- triangle_df %>%
    gisa_extract_triangle(type = "incurred")

  premiums_exposures <- premiums_exposures_df %>%
    group_by(accident_half_year) %>%
    summarize(premium_earned = sum(earned_premium),
              car_years_earned = sum(earned_vehicles),
              .groups = "drop")

  list(
    premiums_exposures = premiums_exposures,
    reported_counts = count_triangle,
    paid_amounts = paid_triangle,
    incurred_amounts = incurred_triangle
  ) %>%
    openxlsx::write.xlsx(output_path)

  invisible(output_path)
}

# Alberta

## AB - TPL-BI

generate_template_data(
  data_auto[["Loss development exhibit - Private Passenger - Third Party Liability - AB"]] %>%
    filter(kind_of_loss_code %in% c("Bodily Injury by any other third party",
                                    "Bodily Injury by passengers in the insured automobile")),
  data_auto[["Loss development exhibit - Private Passenger - Exposures and Premium distribution - AB"]] %>%
    filter(major_coverage_type == "Third Party Liability"),
  "output/Alberta_TPL_BI.xlsx"
)

## AB - Collision

generate_template_data(
  data_auto$`Loss development exhibit - Private Passenger - Collision - AB`,
  data_auto[["Loss development exhibit - Private Passenger - Exposures and Premium distribution - AB"]] %>%
    filter(major_coverage_type == "Collision"),
  "output/Alberta_Collision.xlsx"
)

# Nova Scotia

## NS - TPL-BI

generate_template_data(
  data_auto$`Loss development exhibit - Private Passenger - Third Party Liability - NS` %>%
    filter(grepl("^Bodily Injury", kind_of_loss_code)),
  data_auto$`Loss development exhibit - Private Passenger - Exposures and Premium distribution - NS` %>%
    filter(major_coverage_type == "Third Party Liability"),
  "output/Nova_Scotia_TPL_BI.xlsx"
)
