#' @importFrom dplyr .data
read_header <- function(path) {
  system.file("extdata", path, package = "gisadata") %>%
    readr::read_csv(col_types = "c") %>%
    dplyr::pull(.data$column)
}

#' GISA data headers
#'
#' @keywords internal
gisa_headers <- function() {
  liability_header <- read_header("cleps_header.csv")
  auto_cat_header <- read_header("auto_eps_header.csv")
  list(
    liability = liability_header,
    auto_cat = auto_cat_header,
    auto = auto_cat_header[1:70]
  )
}
