#' Process GISA data
#'
#' @param path Path to directory of CSV files.
#' @return A list of tibbles whose names denote GISA exhibits.
#' @name gisa_process
NULL

utils::globalVariables(".")

#' @rdname gisa_process
#' @export
gisa_process_clsp <- function(path) {
  col_specs <- gisa_col_specs()
  headers <- gisa_headers()

  path %>%
    fs::dir_ls() %>%
    purrr::map(readr::read_csv,
               col_names = headers$liability,
               col_types = col_specs$liability) %>%
    purrr::imap(~ .x %>%
           gisa_rename_cols() %>%
           dplyr::mutate(file = stringr::str_extract(.y, "LIAB\\d{4}")) %>%
           dplyr::left_join(gisa_exhibit_mapping(), by = c("file", "section_number"))) %>%
    purrr::map(~ dplyr::group_split(.x, exhibit)) %>%
    purrr::flatten() %>%
    purrr::set_names(purrr::map_chr(., ~ dplyr::first(.x$exhibit))) %>%
    purrr::map(~ .x %>%
          gisa_select_cols(dplyr::first(.x$format_number)) %>%
          gisa_liab_map_levels())
}


#' @rdname gisa_process
#' @export
gisa_process_auto_dev <- function(path) {
  col_specs <- gisa_col_specs()
  headers <- gisa_headers()

  path %>%
    fs::dir_ls() %>%
    purrr::map(readr::read_csv,
               col_names = headers$auto,
               col_types = col_specs$auto) %>%
    purrr::imap(~ .x %>%
                  gisa_rename_cols() %>%
                  dplyr::mutate(file = stringr::str_extract(.y, "AUTO\\d{4}")) %>%
                  dplyr::left_join(gisa_exhibit_mapping(), by = c("file", "section_number"))) %>%
    purrr::map(~ dplyr::group_split(.x, exhibit)) %>%
    purrr::flatten() %>%
    purrr::set_names(purrr::map_chr(., ~ dplyr::first(.x$exhibit))) %>%
    purrr::map(~ .x %>%
                 gisa_select_cols(dplyr::first(.x$format_number)) %>%
                 gisa_auto_map_levels())
}

#' @rdname gisa_process
#' @export
gisa_process_auto_cat <- function(path) {
  col_specs <- gisa_col_specs()
  headers <- gisa_headers()

  path %>%
    fs::dir_ls() %>%
    purrr::map(readr::read_csv,
               col_names = headers$auto_cat,
               col_types = col_specs$auto_cat) %>%
    purrr::imap(~ .x %>%
                  gisa_rename_cols() %>%
                  dplyr::mutate(file = stringr::str_extract(.y, "AUTO\\d{4}")) %>%
                  dplyr::left_join(gisa_exhibit_mapping(), by = c("file", "section_number"))) %>%
    purrr::map(~ dplyr::group_split(.x, exhibit)) %>%
    purrr::flatten() %>%
    purrr::set_names(
      purrr::map_chr(., ~ paste0(head(.x$exhibit, 1), " - ", head(.x$province, 1)))
    ) %>%
    purrr::map(~ .x %>%
                 gisa_select_cols(dplyr::first(.x$format_number)) %>%
                 gisa_auto_map_levels())
}
