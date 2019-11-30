
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gisadata

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/kasaai/gisadata.svg?branch=master)](https://travis-ci.org/kasaai/gisadata)
[![Codecov test
coverage](https://codecov.io/gh/kasaai/gisadata/branch/master/graph/badge.svg)](https://codecov.io/gh/kasaai/gisadata?branch=master)
<!-- badges: end -->

Workflow functions for tidying up claims data from the General Insurance
Statistical Agency (GISA) of Canada.

## Installation

``` r
remotes::install_github("kasaai/gisadata")
```

## Example

Attach necessary packages:

``` r
library(gisadata)
library(tidyverse)
library(fs)
```

Suppose the data archives are in the `gisa-data` directory:

``` r
dir_ls("gisa-data")
#> gisa-data/Auto Cat Report.zip       gisa-data/Auto Intro.zip            
#> gisa-data/Auto Loss Development.zip gisa-data/CLSP.zip
```

We can extract the CSV files by calling `gisa_unzip()`:

``` r
# By default, files are extracted to a temp directory
extract_dir <- gisa_unzip("gisa-data")
extract_dir
#> [1] "/var/folders/1z/808_xcnx2fx8d9d2l4d8_hqr0000gn/T//RtmpAD7wWt"
```

The necessary metadata for ingesting the files can be obtained from
`gisa_col_specs()` and `gisa_headers()`.

``` r
col_specs <- gisa_col_specs()
headers <- gisa_headers()
headers$liability %>% head()
#> [1] "Written Premium"       "Earned Premium"        "Reported Claim Count" 
#> [4] "Generated Claim Count" "Loss Amount"           "Expense Amount"
```

We can then read and combine the tables as follows:

``` r
data_liab <- extract_dir %>%
  path("CLSP") %>%
  dir_ls() %>%
  map_dfr(read_csv,
          col_names = headers$liability,
          col_types = col_specs$liability) %>%
  gisa_rename_cols()

glimpse(data_liab)
#> Observations: 21,753
#> Variables: 25
#> $ written_premium            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ earned_premium             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ reported_claim_count       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ generated_claim_count      <dbl> 1, 13, 3, 2, 1, 1, 1, 3, 1, 4, 1, 1, …
#> $ loss_amount                <dbl> 1025845, 3191457, 942248, 899434, 600…
#> $ expense_amount             <dbl> 83635, 293601, 115887, 164441, 24515,…
#> $ loss_and_expense_amount    <dbl> 1109480, 3485058, 1058135, 1063875, 6…
#> $ section_number             <chr> "1", "1", "1", "1", "1", "1", "1", "1…
#> $ company_identification     <chr> "000", "000", "000", "000", "000", "0…
#> $ region                     <chr> "2", "2", "2", "2", "2", "2", "2", "2…
#> $ province                   <chr> "Ontario", "Ontario", "Ontario", "Ont…
#> $ valuation_year             <chr> "201812", "201812", "201812", "201812…
#> $ accident_year              <chr> "2014", "2014", "2014", "2014", "2014…
#> $ entry_year                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ major_class                <chr> "B And P SERVICES", "B And P SERVICES…
#> $ coverage_policy_form       <chr> "10", "10", "10", "10", "10", "10", "…
#> $ industry_code              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ policy_type                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ kind_of_loss_code          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ kind_of_loss_indicator     <chr> "1-Bodily Injury", "1-Bodily Injury",…
#> $ paid_outstanding_indicator <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ claim_location             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ type_of_expense            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ policy_limit_group         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ size_of_loss_range_group   <chr> "1,000,001 - 2,000,000", "200,001 - 3…
```

## Contributing

  - Do not commit any (real) data files.
  - Follow [Tidyverse style guide](https://style.tidyverse.org/).
  - Column renaming uses the mapping table under
    [`data-raw`](https://github.com/kasaai/gisadata/tree/master/data-raw).
    If you’re making a change, the accompanying script should be run and
    the resulting `R/sysdata.rda` committed.
  - Column headers are kept in
    [`inst/extdata`](https://github.com/kasaai/gisadata/tree/master/inst/extdata).
  - Sample uncompressed files are kept in
    [`inst/testdata/gisa-data`](https://github.com/kasaai/gisadata/tree/master/inst/testdata/gisa-data).
  - Input types of each type of table are explicitly specified in
    [`R/col-specs.R`](https://github.com/kasaai/gisadata/blob/master/R/col-specs.R).
