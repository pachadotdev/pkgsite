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
#' @param preview Whether to preview the site after building using a local server (requires the `servr` package)
#' @param quiet Whether to suppress build messages
#' @param url Optional URL for the site, used to create CNAME file (e.g., useful for GitHub Pages)
#'
#' @return Invisible `TRUE` if the site was built successfully
#'
#' @examples
#' \dontrun{
#' build_site()
#'
#' # Preview the site after building
#' build_site(preview = TRUE)
#'
#' # Specify a URL to create a CNAME file
#' build_site(url = "yourdomain.com")
#'
#' # Only rebuild if source is newer than destination
#' build_site(lazy = TRUE)
#' }
#'
#' @export
build_site <- function(
  pkg = ".",
  lazy = FALSE,
  preview = FALSE,
  quiet = TRUE,
  url = NULL
) {
  pkg <- as_pkgsite(pkg)

  message(paste0(
    "-- Building minimal site for package ",
    pkg$package,
    " ",
    paste(rep("-", max(0, 80 - nchar(pkg$package) - 38)), collapse = "")
  ))
  message("Reading from: ", pkg$src_path)
  message("Writing to:   ", pkg$dst_path)

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

  build_news(pkg, quiet = quiet, preview = FALSE)

  build_search_index(pkg)

  if (!is.null(url)) {
    write_cname(url, pkg$dst_path)
  }

  message(paste0(
    "-- Finished building minimal site for package ",
    pkg$package,
    " ",
    paste(rep("-", max(0, 80 - nchar(pkg$package) - 46)), collapse = "")
  ))

  if (preview) {
    preview_site(pkg)
  }

  invisible(TRUE)
}
