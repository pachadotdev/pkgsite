#' Render page with minimal template
#'
#' @description
#' Each page uses simple templates: "head", "header", "content", and "footer".
#'
#' @param pkg Path to package to document.
#' @param name Name of the template (e.g. "home", "reference-topic")
#' @param data Data for the template.
#' @param path Location to create file; relative to destination directory.
#' @param quiet If `quiet`, will suppress output messages
#' @export
render_page <- function(
  pkg = ".",
  name,
  data,
  path,
  quiet = FALSE
) {
  pkg <- as_pkgsite(pkg)

  html <- render_page_html(pkg, name = name, data = data, path = path)
  rendered <- as.character(html, options = character())
  write_if_different(pkg, rendered, path, quiet = quiet)
}

#' @importFrom utils modifyList
#' @importFrom xml2 read_html
render_page_html <- function(pkg, name, data = list(), path = "") {
  # Calculate depth for relative paths
  depth <- length(strsplit(path, "/")[[1]]) - 1
  root_path <- if (depth > 0) paste(rep("../", depth), collapse = "") else ""

  data <- modifyList(data_template(pkg, root_path), data)

  # render template components using minimal templates
  pieces <- c("header", "content", "footer")

  templates <- vapply(
    pieces,
    find_minimal_template,
    character(1),
    page_type = name,
    pkg = pkg
  )
  components <- lapply(templates, render_template, data = data)
  names(components) <- pieces

  # render complete layout
  template <- find_minimal_template("layout", page_type = name, pkg = pkg)
  layout_data <- modifyList(data, components)
  rendered <- render_template(template, layout_data)

  # Strip trailing whitespace
  rendered <- gsub(" +\n", "\n", rendered, perl = TRUE)

  read_html(rendered, encoding = "UTF-8")
}

#' @param root_path Relative path to site root (automatically calculated)
#' @export
#' @rdname render_page
data_template <- function(pkg = ".", root_path = "") {
  pkg <- as_pkgsite(pkg)

  # Get reference topics
  topics <- get_reference_topics(pkg)

  # Get articles
  vignettes <- get_vignettes(pkg)

  list(
    package = data_package(pkg),
    site = data_site(pkg, root_path),
    last_updated = paste(
      "Last updated:",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ),
    has_reference = length(topics) > 0,
    reference_topics = if (length(topics) > 0) {
      lapply(topics, function(topic) {
        list(
          filename = paste0(topic, ".html"),
          title = topic
        )
      })
    } else {
      NULL
    },
    has_vignettes = length(vignettes) > 0,
    vignettes = if (length(vignettes) > 0) {
      lapply(vignettes, function(v) {
        list(
          filename = paste0(tools::file_path_sans_ext(v), ".html"),
          title = get_vignette_title(pkg, v)
        )
      })
    } else {
      NULL
    }
  )
}

find_minimal_template <- function(name, page_type, pkg) {
  # Map content pieces to specific templates based on page type
  template_file <- if (name == "content") {
    paste0("content-", page_type, ".html")
  } else {
    paste0(name, ".html")
  }

  # Look for template in the package
  template_path <- system.file(
    "include/site/html",
    template_file,
    package = "pkgsite"
  )

  if (file.exists(template_path)) {
    return(template_path)
  }

  # If no template found, return an error
  stop(
    "Template ",
    template_file,
    " not found in inst/include/site/html/. ",
    "Make sure the package is properly installed with devtools::load_all().",
    call. = FALSE
  )
}

create_simple_template <- function(content) {
  temp_file <- tempfile(fileext = ".html")
  writeLines(content, temp_file)
  return(temp_file)
}

render_template <- function(path, data) {
  template <- readLines(path, warn = FALSE)
  template <- paste(template, collapse = "\n")
  whisker::whisker.render(template, data)
}

data_package <- function(pkg) {
  list(
    name = pkg$package,
    version = pkg$version
  )
}

data_site <- function(pkg, root_path = "") {
  list(
    title = pkg$package,
    root = root_path
  )
}

get_reference_topics <- function(pkg) {
  man_dir <- file.path(pkg$src_path, "man")
  if (!dir.exists(man_dir)) {
    return(character(0))
  }
  man_files <- list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE)
  topics <- tools::file_path_sans_ext(basename(man_files))
  return(topics)
}

get_vignettes <- function(pkg) {
  vignette_dir <- file.path(pkg$src_path, "vignettes")
  if (!dir.exists(vignette_dir)) {
    return(character(0))
  }

  vignettes <- list.files(
    vignette_dir,
    pattern = "\\.(Rmd|rmd|md)$",
    full.names = TRUE
  )
  vignettes <- basename(vignettes)

  # Filter out files starting with _ (child documents)
  vignettes <- vignettes[!grepl("^_", vignettes)]

  return(vignettes)
}

get_vignette_title <- function(pkg, vignette_file) {
  vignette_path <- file.path(pkg$src_path, "vignettes", vignette_file)

  if (!file.exists(vignette_path)) {
    return(tools::file_path_sans_ext(vignette_file))
  }

  # Read the first few lines to get the YAML header
  lines <- readLines(vignette_path, n = 50, warn = FALSE)

  # Find YAML header boundaries
  yaml_start <- which(lines == "---")[1]
  yaml_end <- which(lines == "---")[2]

  if (is.na(yaml_start) || is.na(yaml_end) || yaml_start >= yaml_end) {
    return(tools::file_path_sans_ext(vignette_file))
  }

  # Extract YAML content
  yaml_lines <- lines[(yaml_start + 1):(yaml_end - 1)]

  # Look for VignetteIndexEntry in the vignette section
  vignette_section_start <- which(grepl("^vignette:", yaml_lines))

  if (length(vignette_section_start) == 0) {
    # If no vignette section, try to find title in YAML
    title_line <- grep("^title:", yaml_lines, value = TRUE)
    if (length(title_line) > 0) {
      title <- gsub(
        '^title:\\s*["\']?([^"\']+)["\']?\\s*$',
        '\\1',
        title_line[1]
      )
      return(title)
    }
    return(tools::file_path_sans_ext(vignette_file))
  }

  # Look for VignetteIndexEntry after the vignette section
  remaining_lines <- yaml_lines[vignette_section_start:length(yaml_lines)]
  entry_line <- grep("VignetteIndexEntry", remaining_lines, value = TRUE)

  if (length(entry_line) > 0) {
    # Extract the entry text - handle both {Title} and "Title" formats
    title <- gsub('.*VignetteIndexEntry\\{([^}]+)\\}.*', '\\1', entry_line[1])
    if (title == entry_line[1]) {
      # Try alternative format with quotes/braces
      title <- gsub(
        '.*VignetteIndexEntry[{"]([^}"]+)[}"].*',
        '\\1',
        entry_line[1]
      )
    }
    return(title)
  }

  # Fallback to YAML title if available
  title_line <- grep("^title:", yaml_lines, value = TRUE)
  if (length(title_line) > 0) {
    title <- gsub('^title:\\s*["\']?([^"\']+)["\']?\\s*$', '\\1', title_line[1])
    return(title)
  }

  # Final fallback to filename
  return(tools::file_path_sans_ext(vignette_file))
}

write_if_different <- function(pkg, contents, path, quiet = FALSE) {
  full_path <- file.path(pkg$dst_path, path)

  if (!quiet) {
    message("Writing ", path)
  }

  dir.create(dirname(full_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(contents, full_path)
}
