#' Build a minimal pkgdown website
#'
#' @description
#' `build_site()` creates a simple static documentation site with:
#'
#' * [init_site()]
#' * [build_home()]
#' * [build_reference()]
#' * [build_vignettes()]
#'
#' The site uses a minimal template with a simple sidebar navigation.
#'
#' @param pkg Path to package
#' @param lazy If `TRUE`, will only rebuild if source is newer than destination
#' @param preview Whether to preview the site after building
#' @param quiet Whether to suppress build messages
#' @export
#' @examples
#' \dontrun{
#'  build_site()
#' }
build_site <- function(
  pkg = ".",
  lazy = FALSE,
  preview = FALSE,
  quiet = TRUE
) {
  pkg <- as_pkgsite(pkg)

  cli::cli_rule(
    "Building minimal site for package {.pkg {pkg$package}}"
  )
  cli::cli_inform("Reading from: {.path {pkg$src_path}}")
  cli::cli_inform("Writing to:   {.path {pkg$dst_path}}")

  if (!lazy) {
    init_site(pkg)
  }

  build_home(pkg, quiet = quiet, preview = FALSE)
  build_reference(pkg, lazy = lazy, preview = FALSE)
  build_vignettes(
    pkg,
    lazy = lazy,
    quiet = quiet,
    preview = FALSE
  )

  build_search_index(pkg)

  cli::cli_rule(
    "Finished building minimal site for package {.pkg {pkg$package}}"
  )

  if (preview) {
    preview_site(pkg)
  }

  invisible(pkg)
}
