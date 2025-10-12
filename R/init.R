#' Initialise minimal site infrastructure
#'
#' @description
#' `init_site()` creates the output directory and copies basic CSS files.
#'
#' @param pkg Path to package
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

  # Copy JavaScript files from inst/include/site/js
  js_assets_path <- system.file("include/site/js", package = "pkgsite")
  if (dir.exists(js_assets_path)) {
    # Create js directory in destination if it doesn't exist
    js_dst_path <- file.path(pkg$dst_path, "js")
    if (!dir.exists(js_dst_path)) {
      dir.create(js_dst_path, recursive = TRUE)
    }

    js_files <- list.files(
      js_assets_path,
      pattern = "\\.js$",
      full.names = TRUE
    )
    for (js_file in js_files) {
      file.copy(
        js_file,
        file.path(js_dst_path, basename(js_file)),
        overwrite = TRUE
      )
    }
  }

  # Copy logo files if they exist
  logo_locations <- c(
    "man/figures/logo.svg",
    "man/figures/logo.png",
    "man/figures/hexlogo.svg",
    "man/figures/hexlogo.png",
    "logo.svg",
    "logo.png"
  )

  for (logo_path in logo_locations) {
    logo_src <- file.path(pkg$src_path, logo_path)
    if (file.exists(logo_src)) {
      # Create the destination directory structure
      logo_dst <- file.path(pkg$dst_path, logo_path)
      dir.create(dirname(logo_dst), recursive = TRUE, showWarnings = FALSE)

      file.copy(
        logo_src,
        logo_dst,
        overwrite = TRUE
      )
      break # Only copy the first logo found
    }
  }

  invisible()
}
