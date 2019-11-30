#' Rename columns
#'
#' @param data A data frame with GISA column names.
#' @export
gisa_rename_cols <- function(data) {
  cols_to_rename <- tibble::tibble(old_name = names(data)) %>%
    dplyr::inner_join(gisa_column_mapping, by = "old_name")

  if (nrow(cols_to_rename)) {
    renames <- cols_to_rename$old_name %>%
      stats::setNames(cols_to_rename$new_name)

    data %>%
      dplyr::rename(!!renames)
  } else {
    warning("No columns renamed by `gisa_rename_cols()`.", call. = FALSE)
    data
  }
}
