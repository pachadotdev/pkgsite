#' Generate minimal pkgsite data structure
#'
#' @param pkg Path to package.
as_pkgsite <- function(pkg = ".") {
  if (is_pkgsite(pkg)) {
    return(pkg)
  }

  # Expand tilde and resolve to absolute path
  pkg <- path.expand(pkg)
  pkg <- normalizePath(pkg, mustWork = FALSE)

  if (!dir.exists(pkg)) {
    stop(pkg, " is not an existing directory", call. = FALSE)
  }

  src_path <- pkg

  # Read DESCRIPTION file using base R
  desc_path <- file.path(src_path, "DESCRIPTION")
  if (!file.exists(desc_path)) {
    stop("DESCRIPTION file not found in ", src_path, call. = FALSE)
  }

  desc_fields <- read.dcf(desc_path)[1, ]

  # Read basic meta config
  meta_path <- file.path(src_path, "_pkgdown.yml")
  if (file.exists(meta_path)) {
    meta <- yaml::read_yaml(meta_path)
  } else {
    meta <- list()
  }

  # Create minimal pkgsite object
  pkg <- list(
    package = desc_fields[["Package"]],
    version = desc_fields[["Version"]],
    src_path = src_path,
    dst_path = file.path(src_path, meta$destination %||% "docs"),
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

  site_path <- file.path(pkg$dst_path, path)
  if (file.exists(site_path)) {
    message("Opening site in browser: ", site_path)

    # Open the HTML file directly in the browser
    if (.Platform$OS.type == "windows") {
      system(paste("start", shQuote(site_path)))
    } else if (Sys.info()["sysname"] == "Darwin") {
      system(paste("open", shQuote(site_path)))
    } else {
      # Linux and other Unix-like systems
      system(paste("xdg-open", shQuote(site_path)))
    }
  } else {
    message("Site file not found: ", site_path)
  }
  invisible()
}


# Helper operator
`%||%` <- function(a, b) if (is.null(a)) b else a
