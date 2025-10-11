#' Build home section
#'
#' @description
#' `build_home()` generates the home page from README.md or index.md
#'
#' @param pkg Path to package
#' @param quiet Whether to suppress messages
#' @param preview Whether to preview after building
#' @export
build_home <- function(
  pkg = ".",
  quiet = FALSE,
  preview = FALSE
) {
  pkg <- as_pkgsite(pkg)

  if (!quiet) {
    cli::cli_rule("Building home")
  }

  # Build index page
  build_home_index(pkg, quiet = quiet)

  if (preview) {
    preview_site(pkg, "index.html")
  }

  invisible(pkg)
}

#' @export
#' @rdname build_home
build_home_index <- function(pkg = ".", quiet = FALSE) {
  pkg <- as_pkgsite(pkg)

  # Find home page source
  home_source <- find_home_source(pkg)

  if (!quiet) {
    if (!is.null(home_source)) {
      cli::cli_inform("Reading {.file {home_source}}")
    } else {
      cli::cli_inform("No README found, creating basic home page")
    }
    cli::cli_inform("Writing {.file index.html}")
  }

  # Read and convert markdown content
  if (!is.null(home_source) && file.exists(home_source)) {
    content <- markdown_to_html(home_source)
    # Copy images referenced in README
    copy_readme_images(pkg, home_source)
  } else {
    content <- paste0(
      "<h1>",
      pkg$package,
      "</h1>",
      "<p>Package documentation</p>"
    )
  }

  data <- list(
    pagetitle = paste(pkg$package, "documentation"),
    contents = content
  )

  render_page(pkg, "home", data, "index.html", quiet = quiet)
  invisible(pkg)
}

find_home_source <- function(pkg) {
  candidates <- c(
    fs::path(pkg$src_path, "pkgdown", "index.md"),
    fs::path(pkg$src_path, "index.md"),
    fs::path(pkg$src_path, "README.md")
  )

  for (candidate in candidates) {
    if (file.exists(candidate)) {
      return(candidate)
    }
  }

  return(NULL)
}

markdown_to_html <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    return("")
  }

  # Read content as single string for better regex processing
  content <- readLines(path, warn = FALSE)
  content <- paste(content, collapse = "\n")

  # Convert markdown images: ![alt](url)
  content <- gsub(
    "!\\[([^\\]]*)\\]\\(([^\\)]+)\\)",
    "<img src=\"\\2\" alt=\"\\1\">",
    content,
    perl = TRUE
  )

  # Convert markdown links: [text](url)
  content <- gsub(
    "\\[([^\\]]+)\\]\\(([^\\)]+)\\)",
    "<a href=\"\\2\">\\1</a>",
    content,
    perl = TRUE
  )

  # Convert inline code: `code`
  content <- gsub(
    "`([^`]+)`",
    "<code>\\1</code>",
    content,
    perl = TRUE
  )

  # Process line by line for headers
  lines <- strsplit(content, "\n")[[1]]
  html_lines <- character(length(lines))

  for (i in seq_along(lines)) {
    line <- lines[i]

    # Handle headers
    if (grepl("^# ", line)) {
      html_lines[i] <- gsub("^# (.+)$", "<h1>\\1</h1>", line)
    } else if (grepl("^## ", line)) {
      html_lines[i] <- gsub("^## (.+)$", "<h2>\\1</h2>", line)
    } else if (grepl("^### ", line)) {
      html_lines[i] <- gsub("^### (.+)$", "<h3>\\1</h3>", line)
    } else if (grepl("^#### ", line)) {
      html_lines[i] <- gsub("^#### (.+)$", "<h4>\\1</h4>", line)
    } else if (line == "") {
      # Empty lines become paragraph breaks
      html_lines[i] <- ""
    } else {
      # Regular text lines
      html_lines[i] <- line
    }
  }

  # Convert to paragraphs
  content <- paste(html_lines, collapse = "\n")

  # Split by double newlines to create paragraphs
  paragraphs <- strsplit(content, "\n\n")[[1]]
  paragraphs <- paragraphs[paragraphs != ""]

  # Filter out HTML comments and empty paragraphs
  paragraphs <- paragraphs[!grepl("^\\s*<!--.*-->\\s*$", paragraphs)]
  paragraphs <- paragraphs[paragraphs != ""]

  # Wrap non-header content in paragraphs
  for (i in seq_along(paragraphs)) {
    para <- paragraphs[i]
    # Skip if it's already a header or empty
    if (!grepl("^<h[1-6]", para) && para != "") {
      paragraphs[i] <- paste0("<p>", para, "</p>")
    }
  }

  return(paste(paragraphs, collapse = "\n\n"))
}

copy_readme_images <- function(pkg, readme_path) {
  if (!file.exists(readme_path)) {
    return(invisible())
  }

  # Read README content
  readme_content <- readLines(readme_path, warn = FALSE)
  readme_text <- paste(readme_content, collapse = "\n")

  # Find image references in markdown format: ![alt](path)
  image_matches <- gregexpr(
    '!\\[[^\\]]*\\]\\(([^\\)]+)\\)',
    readme_text,
    perl = TRUE
  )
  image_paths <- character(0)

  if (image_matches[[1]][1] != -1) {
    match_data <- regmatches(readme_text, image_matches)[[1]]
    for (match in match_data) {
      # Extract the path from ![alt](path)
      path <- gsub('!\\[[^\\]]*\\]\\(([^\\)]+)\\)', '\\1', match, perl = TRUE)
      # Skip URLs (http/https)
      if (!grepl("^https?://", path)) {
        image_paths <- c(image_paths, path)
      }
    }
  }

  # Also check for HTML img tags: <img src="path" ...>
  html_matches <- gregexpr('<img[^>]+src="([^"]+)"', readme_text, perl = TRUE)
  if (html_matches[[1]][1] != -1) {
    match_data <- regmatches(readme_text, html_matches)[[1]]
    for (match in match_data) {
      path <- gsub('<img[^>]+src="([^"]+)".*', '\\1', match, perl = TRUE)
      # Skip URLs
      if (!grepl("^https?://", path)) {
        image_paths <- c(image_paths, path)
      }
    }
  }

  # Copy each image to docs folder
  for (image_path in image_paths) {
    src_path <- fs::path(pkg$src_path, image_path)

    if (file.exists(src_path)) {
      # Create destination directory structure
      dest_path <- fs::path(pkg$dst_path, image_path)
      dest_dir <- fs::path_dir(dest_path)

      if (!fs::dir_exists(dest_dir)) {
        fs::dir_create(dest_dir, recurse = TRUE)
      }

      # Copy the image
      fs::file_copy(src_path, dest_path, overwrite = TRUE)
      cli::cli_inform("Copying {.file {image_path}}")
    } else {
      cli::cli_warn("Image not found: {.file {src_path}}")
    }
  }

  invisible()
}
