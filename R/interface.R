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
  dfs <- process_files(
    path = path,
    col_names = gisa_headers()$auto,
    col_types = gisa_col_specs()$auto,
    file_regex = "AUTO\\d{4}",
    categorical_mapper = gisa_auto_map_levels
  )

  loss_development_exhibits <- dfs %>%
    names() %>%
    purrr::discard(~ grepl("Exposures and Premium distribution", .x))

  dfs %>%
    purrr::map_at(
      loss_development_exhibits,
      ~ .x %>%
        dplyr::filter(as.numeric(.data$entry_half_year) >=
                        as.numeric(.data$accident_half_year)) %>%
        gisa_origin_dev(.data$accident_half_year, .data$entry_half_year)
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

#' Compute origin and development periods
#'
#' @param data Data frame with loss development data.
#' @param accident_col Column corresponding to accident half year, as an unquoted string.
#' @param entry_col Column corresponding to entry half year, as an unquoted string.
#' @keywords internal
gisa_origin_dev <- function(data, accident_col, entry_col) {
  accident_col <- rlang::enquo(accident_col)
  entry_col <- rlang::enquo(entry_col)
  data %>%
    dplyr::mutate(
      origin_period_end = end_of_period(!!accident_col),
      development_period_end = end_of_period(!!entry_col),
      development_month = round(
        lubridate::interval(.data$origin_period_end, .data$development_period_end) /
          lubridate::duration(1, "months")
      ) %>%
        as.integer() %>%
        `+`(6L)
    )
}

end_of_period <- function(x) {
  yr <- substr(x, 1, 4)
  half <- substr(x, 5, 6)
  lubridate::ymd(paste(yr, as.integer(half) * 6, "01", sep = "-")) %>%
    lubridate::ceiling_date("months") %>%
    `-`(lubridate::days(1))
}

compute_report_date <- function(accident_half_year, development_month) {
  md <- ifelse(substr(accident_half_year, 5, 6) == "01", "0630", "1231")
  lubridate::ymd(paste0(substr(accident_half_year, 1, 4), md)) %m+%
    months(development_month)
}

generate_dev_months <- function(x) {
  seq_len(max(x) / 6) * 6
}

#' Extract triangle
#'
#' @param data A loss development exhibit table.
#' @param type Either "paid" or "incurred".
#' @importFrom lubridate ymd
#' @export
gisa_extract_triangle <- function(data, type = c("paid", "incurred"),
                                  evaluation_date = ymd("2018-12-31")) {
  type <- match.arg(type)

  triangle_data <- data %>%
    dplyr::filter(.data$paid_outstanding_indicator == "Paid") %>%
    dplyr::group_by(.data$accident_half_year, .data$development_month) %>%
    dplyr::summarize(paid_loss = sum(.data$loss_amount)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(.data$accident_half_year,
                    development_month = generate_dev_months(.data$development_month),
                    fill = list(paid_loss = 0)) %>%
    dplyr::mutate(
      report_date = compute_report_date(.data$accident_half_year, .data$development_month)
    ) %>%
    dplyr::filter(report_date <= evaluation_date) %>%
    dplyr::group_by(.data$accident_half_year) %>%
    dplyr::arrange(.data$accident_half_year, .data$development_month) %>%
    dplyr::mutate(loss = cumsum(.data$paid_loss))

  if (identical(type, "incurred")) {
    outstanding <- data %>%
      dplyr::filter(.data$paid_outstanding_indicator == "Outstanding") %>%
      dplyr::group_by(.data$accident_half_year, .data$development_month) %>%
      dplyr::summarize(outstanding = sum(.data$loss_amount)) %>%
      dplyr::select(.data$accident_half_year, .data$development_month, .data$outstanding)

    triangle_data <- triangle_data %>%
      dplyr::left_join(outstanding, by = c("accident_half_year", "development_month")) %>%
      dplyr::mutate_at("outstanding", ~ ifelse(is.na(.x), 0, .x)) %>%
      dplyr::mutate(loss = .data$loss + .data$outstanding)
  }

  triangle <- triangle_data %>%
    dplyr::select(.data$accident_half_year, .data$development_month, .data$loss) %>%
    tidyr::pivot_wider(names_from = .data$development_month, values_from = .data$loss,
                       values_fill = list(loss = 0))

  dev_months <- triangle %>%
    names() %>%
    setdiff("accident_half_year") %>%
    as.integer() %>%
    sort()

  triangle %>%
    dplyr::select(.data$accident_half_year, as.character(!!dev_months)) %>%
    dplyr::ungroup()
}
