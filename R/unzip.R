#' Extract zip files
#'
#' Extract files, up to two layers of .zip files, from a directory into a
#'   specified directory.
#'
#' @param zips_dir The directory of the zip files.
#' @param extract_dir The directory to extract to.
#' @export
gisa_unzip <- function(zips_dir, extract_dir = tempdir()) {

  zips_dir %>%
    fs::dir_ls(glob = "*zip") %>%
    purrr::map(utils::unzip, junkpaths = FALSE, exdir = extract_dir) %>%
    purrr::flatten_chr() %>%
    purrr::keep(~ tools::file_ext(.x) == "zip") %>%
    purrr::walk(utils::unzip, junkpaths = TRUE, exdir = extract_dir)

  fs::dir_ls(extract_dir, glob = "*CSV", invert = TRUE) %>%
    purrr::walk(fs::file_delete)

  invisible(extract_dir)
}
