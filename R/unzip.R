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
    purrr::walk(~ utils::unzip(
      .x,
      junkpaths = TRUE,
      exdir = fs::path(extract_dir,
                       fs::path_file(fs::path_ext_remove(.x)))
    ))

  extract_dir %>%
    fs::dir_ls(glob = "*zip", recurse = TRUE) %>%
    purrr::walk(~ utils::unzip(.x, junkpaths = TRUE,
                               exdir = fs::path_dir(.x)))

  fs::dir_ls(extract_dir, glob = "*CSV", type = "file",
             invert = TRUE, recurse = TRUE) %>%
    purrr::walk(fs::file_delete)

  invisible(extract_dir)
}
