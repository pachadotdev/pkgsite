#' Initialise minimal site infrastructure
#'
#' @description
#' `init_site()` creates the output directory and copies basic CSS files.
#'
#' @param pkg Path to package
#' @export
init_site <- function(pkg = ".") {
  pkg <- as_pkgsite(pkg)

  cli::cli_rule("Initialising minimal site")
  fs::dir_create(pkg$dst_path)

  # Copy minimal assets
  copy_minimal_assets(pkg)

  invisible()
}

copy_minimal_assets <- function(pkg = ".") {
  pkg <- as_pkgsite(pkg)

  # Copy minimal CSS files from inst/include/site/css
  css_assets_path <- system.file("include/site/css", package = "pkgsite")
  if (fs::dir_exists(css_assets_path)) {
    css_files <- fs::dir_ls(css_assets_path, glob = "*.css")
    for (css_file in css_files) {
      fs::file_copy(
        css_file,
        fs::path(pkg$dst_path, fs::path_file(css_file)),
        overwrite = TRUE
      )
    }
  }

  invisible()
}
