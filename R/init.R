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

  # Preserve CNAME file if it exists
  cname_path <- file.path(pkg$dst_path, "CNAME")
  cname_backup <- NULL
  if (file.exists(cname_path)) {
    cname_backup <- readLines(cname_path, warn = FALSE)
    message("Preserving existing CNAME file")
  }

  dir.create(pkg$dst_path, recursive = TRUE, showWarnings = FALSE)

  # Restore CNAME file if it was backed up
  if (!is.null(cname_backup)) {
    writeLines(cname_backup, cname_path)
  }

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
