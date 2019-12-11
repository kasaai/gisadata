
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
#> [1] "/var/folders/lm/wwpd13g55cz3wf0gn594b59w0000gn/T//Rtmpr2LsED"
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

Read and process tables:

``` r
data_liab <- extract_dir %>%
  path("CLSP") %>%
  dir_ls() %>%
  map(read_csv,
          col_names = headers$liability,
          col_types = col_specs$liability) %>%
  imap(~ .x %>%
         gisa_rename_cols() %>%
         mutate(file = str_extract(.y, "LIAB\\d{4}")) %>%
         left_join(gisa_exhibit_mapping(), by = c("file", "section_number"))) %>%
  map(~ group_split(.x, exhibit)) %>%
  flatten() %>%
  set_names(map_chr(., ~ first(.x$exhibit))) %>%
  map(~ .x %>% 
        gisa_select_cols(first(.x$format_number)) %>% 
        gisa_liab_map_levels())

data_liab$`Loss Development - CGL - with product/completed operations` %>% 
  glimpse()
#> Observations: 5,082
#> Variables: 16
#> $ generated_claim_count      <dbl> 3.000, 0.000, 1.000, 6.000, 6.000, 1.000, …
#> $ loss_amount                <dbl> 69592, 0, 39824, 443458, 514070, 11688, 35…
#> $ expense_amount             <dbl> 27151, 3764, 0, 10569, 149171, 6332, 16858…
#> $ loss_and_expense_amount    <dbl> 96743, 3764, 39824, 454027, 663241, 18020,…
#> $ section_number             <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1…
#> $ company_identification     <chr> "000", "000", "000", "000", "000", "000", …
#> $ region                     <chr> "2", "2", "2", "2", "2", "2", "2", "2", "2…
#> $ province                   <chr> "Ontario", "Ontario", "Ontario", "Ontario"…
#> $ valuation_year             <chr> "201812", "201812", "201812", "201812", "2…
#> $ accident_year              <chr> "2004", "2004", "2004", "2004", "2004", "2…
#> $ entry_year                 <chr> "2004", "2004", "2004", "2004", "2004", "2…
#> $ policy_type                <chr> "Occurrence", "Occurrence", "Occurrence", …
#> $ kind_of_loss_indicator     <chr> "1-Bodily Injury", "1-Bodily Injury", "1-B…
#> $ coverage_policy_form       <chr> "CGL - with product/completed operations",…
#> $ kind_of_loss               <chr> "Bodily Injury - Employers' Liability", "B…
#> $ paid_outstanding_indicator <chr> "Outstanding", "Paid", "Outstanding", "Pai…
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
