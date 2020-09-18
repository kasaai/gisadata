# Select HRGs for analysis – I propose the following ten HRGs:
#
#   * Ontario (ON)
#     * TPL - BI
#     * AB - MR
#     * AB - DI
#     * CL
#
#   * Alberta (AB)
#     * TPL - BI
#     * CL
#
#   * Nova Scotia (NS)
#     * TPL-BI
#     * CL
#
#   * Atlantic Canada combined (NS, NL, PE, NB)
#     * TPL-BI
#     * CL

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


# Province names split ----------------------------------------------------------
province_list <- names(data_auto) %>% str_extract("[:upper:]+$") %>% unique()

AB_names <- names(data_auto) %>% magrittr::extract(.,str_detect(.,"AB$"))
NB_names <- names(data_auto) %>% magrittr::extract(.,str_detect(.,"NB$"))
NL_names <- names(data_auto) %>% magrittr::extract(.,str_detect(.,"NL$"))
NS_names <- names(data_auto) %>% magrittr::extract(.,str_detect(.,"NS$"))
PE_names <- names(data_auto) %>% magrittr::extract(.,str_detect(.,"PE$"))
ON_names <- names(data_auto) %>% magrittr::extract(.,str_detect(.,"ON$"))

# Data Extraction ---------------------------------------------------------

# Ontario (ON)

ON_names

## ON - TPL-BI

generate_template_data(
  data_auto[["Loss development exhibit - Private Passenger - Third Party Liability - ON"]] %>%
    filter(grepl("^Bodily Injury", kind_of_loss_code)),
  data_auto[["Loss development exhibit - Private Passenger - Exposures and Premium distribution - ON"]] %>%
    filter(major_coverage_type == "Third Party Liability"),
  "output/Ontario_TPL_BI.xlsx"
)

## ON - AB-MR

generate_template_data(
  data_auto[["Loss development exhibit - Private Passenger - Accident Benefits - ON"]] %>%
    filter(kind_of_loss_code %in% c("Medical, excluding rehabilitation and extended care",
                                    "Rehabilitation - other than renovations",
                                    "Rehabilitation - renovations")),
  data_auto[["Loss development exhibit - Private Passenger - Exposures and Premium distribution - ON"]] %>%
    filter(major_coverage_type == "Accident Benefits"),
  "output/Ontario_AB_MR.xlsx"
)

## ON - AB-DI

generate_template_data(
  data_auto[["Loss development exhibit - Private Passenger - Accident Benefits - ON"]] %>%
    filter(grepl("^.*disability.*$", kind_of_loss_code)),
  data_auto[["Loss development exhibit - Private Passenger - Exposures and Premium distribution - ON"]] %>%
    filter(major_coverage_type == "Accident Benefits"),
  "output/Ontario_AB_DI.xlsx"
)

## ON - CL

generate_template_data(
  data_auto[["Loss development exhibit - Private Passenger - Collision - ON"]],
  data_auto[["Loss development exhibit - Private Passenger - Exposures and Premium distribution - ON"]] %>%
    filter(major_coverage_type == "Collision"),
  "output/Ontario_Collision.xlsx"
)

# Alberta

AB_names

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
  data_auto[["Loss development exhibit - Private Passenger - Collision - AB"]],
  data_auto[["Loss development exhibit - Private Passenger - Exposures and Premium distribution - AB"]] %>%
    filter(major_coverage_type == "Collision"),
  "output/Alberta_Collision.xlsx"
)

# Nova Scotia

NS_names

## NS - TPL-BI

generate_template_data(
  data_auto[["Loss development exhibit - Private Passenger - Third Party Liability - NS"]] %>%
    filter(grepl("^Bodily Injury", kind_of_loss_code)),
  data_auto$`Loss development exhibit - Private Passenger - Exposures and Premium distribution - NS` %>%
    filter(major_coverage_type == "Third Party Liability"),
  "output/Nova_Scotia_TPL_BI.xlsx"
)


## NS - Collision

generate_template_data(
  data_auto[["Loss development exhibit - Private Passenger - Collision - NS"]],
  data_auto[["Loss development exhibit - Private Passenger - Exposures and Premium distribution - NS"]] %>%
    filter(major_coverage_type == "Collision"),
  "output/Nova_Scotia_Collision.xlsx"
)


##  Atlantic Canada: NS, NL, PE, NB -

c(NS_names, NL_names, PE_names, NB_names)

# AC - TPL-BI

all.equal(names(data_auto[["Loss development exhibit - Private Passenger - Third Party Liability - NS"]]),
          names(data_auto[["Loss development exhibit - Private Passenger - Third Party Liability - NL"]]),
          names(data_auto[["Loss development exhibit - Private Passenger - Third Party Liability - PE"]]),
          names(data_auto[["Loss development exhibit - Private Passenger - Third Party Liability - NB"]]))

data_auto[["Loss development exhibit - Private Passenger - Third Party Liability - AC"]] <-
  union(data_auto[["Loss development exhibit - Private Passenger - Third Party Liability - NS"]],
        data_auto[["Loss development exhibit - Private Passenger - Third Party Liability - NL"]],
        data_auto[["Loss development exhibit - Private Passenger - Third Party Liability - PE"]],
        data_auto[["Loss development exhibit - Private Passenger - Third Party Liability - NB"]])

all.equal(names(data_auto[["Loss development exhibit - Private Passenger - Exposures and Premium distribution - NS"]]),
          names(data_auto[["Loss development exhibit - Private Passenger - Exposures and Premium distribution - NL"]]),
          names(data_auto[["Loss development exhibit - Private Passenger - Exposures and Premium distribution - PE"]]),
          names(data_auto[["Loss development exhibit - Private Passenger - Exposures and Premium distribution - NB"]]))

data_auto[["Loss development exhibit - Private Passenger - Exposures and Premium distribution - AC"]] <-
  union(data_auto[["Loss development exhibit - Private Passenger - Exposures and Premium distribution - NS"]],
        data_auto[["Loss development exhibit - Private Passenger - Exposures and Premium distribution - NL"]],
        data_auto[["Loss development exhibit - Private Passenger - Exposures and Premium distribution - PE"]],
        data_auto[["Loss development exhibit - Private Passenger - Exposures and Premium distribution - NB"]])

generate_template_data(
  data_auto[["Loss development exhibit - Private Passenger - Third Party Liability - AC"]] %>%
    filter(grepl("^Bodily Injury.*$", kind_of_loss_code)),
  data_auto[["Loss development exhibit - Private Passenger - Exposures and Premium distribution - AC"]] %>%
    filter(major_coverage_type == "Third Party Liability"),
  "output/Atlantic_Canada_TPL_BI.xlsx"
)

# AC - CL

all.equal(names(data_auto[["Loss development exhibit - Private Passenger - Collision - NS"]]),
          names(data_auto[["Loss development exhibit - Private Passenger - Collision - NL"]]),
          names(data_auto[["Loss development exhibit - Private Passenger - Collision - PE"]]),
          names(data_auto[["Loss development exhibit - Private Passenger - Collision - NB"]]))

data_auto[["Loss development exhibit - Private Passenger - Collision - AC"]] <-
  union(data_auto[["Loss development exhibit - Private Passenger - Collision - NS"]],
        data_auto[["Loss development exhibit - Private Passenger - Collision - NL"]],
        data_auto[["Loss development exhibit - Private Passenger - Collision - PE"]],
        data_auto[["Loss development exhibit - Private Passenger - Collision - NB"]])

generate_template_data(
  data_auto[["Loss development exhibit - Private Passenger - Collision - AC"]],
  data_auto[["Loss development exhibit - Private Passenger - Exposures and Premium distribution - AC"]] %>%
    filter(major_coverage_type == "Collision"),
  "output/Atlantic_Canada_Collision.xlsx"
)

