#' Extract zip files
#'
#' Extract files, up to two layers of .zip files, from a directory into a
#'   specified directory.
#'
#' @param zips_dir The directory of the zip files.
#' @param extract_dir The directory to extract to.
#' @export
gisa_unzip <- function(zips_dir, extract_dir = tempdir()) {

  extract_dir <- fs::dir_copy(zips_dir, tempdir())

  extract_dir %>%
    fs::dir_ls(glob = "*zip") %>%
    purrr::map(utils::unzip, exdir = extract_dir) %>%
    purrr::flatten_chr() %>%
    purrr::keep(~ tools::file_ext(.x) == "zip") %>%
    purrr::walk(~ utils::unzip(.x, exdir = path_dir(.x)))

  fs::dir_ls(extract_dir, glob = "*CSV", type = "file",
             invert = TRUE, recurse = TRUE) %>%
    purrr::walk(fs::file_delete)

  invisible(extract_dir)
}
