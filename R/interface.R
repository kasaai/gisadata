#' Process GISA data
#'
#' @param path Path to directory of CSV files.
#' @return A list of tibbles whose names denote GISA exhibits.
#' @name gisa_process
NULL

utils::globalVariables(".")

process_files <- function(path, col_names, col_types, file_regex, categorical_mapper) {
  path %>%
    fs::dir_ls() %>%
    purrr::map(readr::read_csv,
               col_names = col_names,
               col_types = col_types) %>%
    purrr::imap(~ .x %>%
                  gisa_rename_cols() %>%
                  dplyr::mutate(file = stringr::str_extract(.y, file_regex)) %>%
                  dplyr::left_join(gisa_exhibit_mapping(), by = c("file", "section_number"))) %>%
    purrr::map(~ dplyr::group_split(.x, exhibit, province)) %>%
    purrr::flatten() %>%
    purrr::set_names(
      purrr::map_chr(., ~ paste0(unique(.x$exhibit), " - ", unique(.x$province)))
    ) %>%
    purrr::map(~ .x %>%
                 gisa_select_cols(unique(.x$format_number)) %>%
                 categorical_mapper())
}

#' @rdname gisa_process
#' @export
gisa_process_clsp <- function(path) {
  process_files(
    path = path,
    col_names = gisa_headers()$liability,
    col_types = gisa_col_specs()$liability,
    file_regex = "LIAB\\d{4}",
    categorical_mapper = gisa_liab_map_levels
  )
}


#' @rdname gisa_process
#' @export
gisa_process_auto_dev <- function(path) {
  process_files(
    path = path,
    col_names = gisa_headers()$auto,
    col_types = gisa_col_specs()$auto,
    file_regex = "AUTO\\d{4}",
    categorical_mapper = gisa_auto_map_levels
  )
}

#' @rdname gisa_process
#' @export
gisa_process_auto_cat <- function(path) {
  process_files(
    path = path,
    col_names = gisa_headers()$auto_cat,
    col_types = gisa_col_specs()$auto_cat,
    file_regex = "AUTO\\d{4}",
    categorical_mapper = gisa_auto_map_levels
  )
}
