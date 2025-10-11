#' Generate minimal pkgsite data structure
#'
#' @param pkg Path to package.
as_pkgsite <- function(pkg = ".") {
  if (is_pkgsite(pkg)) {
    return(pkg)
  }

  if (!fs::dir_exists(pkg)) {
    cli::cli_abort("{.file {pkg}} is not an existing directory")
  }

  src_path <- pkg
  desc <- desc::desc(src_path)

  # Read basic meta config
  meta_path <- fs::path(src_path, "_pkgdown.yml")
  if (fs::file_exists(meta_path)) {
    meta <- yaml::read_yaml(meta_path)
  } else {
    meta <- list()
  }

  # Create minimal pkgsite object
  pkg <- list(
    package = desc$get_field("Package"),
    version = desc$get_field("Version"),
    src_path = src_path,
    dst_path = fs::path(src_path, meta$destination %||% "docs"),
    meta = meta
  )

  class(pkg) <- "pkgsite"
  pkg
}

is_pkgsite <- function(x) {
  inherits(x, "pkgsite")
}

preview_site <- function(pkg, path = NULL) {
  if (is.null(path)) {
    path <- "index.html"
  }

  site_path <- fs::path(pkg$dst_path, path)
  if (fs::file_exists(site_path)) {
    cli::cli_inform("Preview site at {.file {site_path}}")
  }
  invisible()
}
