#' Initialise minimal site infrastructure
#'
#' @description
#' `init_site()` creates the output directory and copies basic CSS files.
#'
#' @param pkg Path to package
#' @export
init_site <- function(pkg = ".") {
  pkg <- as_pkgsite(pkg)

  message(paste0(
    "-- Initialising minimal site ",
    paste(rep("-", 54), collapse = "")
  ))
  dir.create(pkg$dst_path, recursive = TRUE, showWarnings = FALSE)

  # Copy minimal assets
  copy_minimal_assets(pkg)

  invisible()
}

copy_minimal_assets <- function(pkg = ".") {
  pkg <- as_pkgsite(pkg)

  # Copy minimal CSS files from inst/include/site/css
  css_assets_path <- system.file("include/site/css", package = "pkgsite")
  if (dir.exists(css_assets_path)) {
    css_files <- list.files(
      css_assets_path,
      pattern = "\\.css$",
      full.names = TRUE
    )
    for (css_file in css_files) {
      file.copy(
        css_file,
        file.path(pkg$dst_path, basename(css_file)),
        overwrite = TRUE
      )
    }
  }

  invisible()
}
