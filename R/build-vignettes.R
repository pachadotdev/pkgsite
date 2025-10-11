#' Build vignettes section
#'
#' @description
#' `build_vignettes()` renders R Markdown files from vignettes/ to vignettes/
#'
#' @param pkg Path to package
#' @param lazy Whether to skip files that haven't changed
#' @param quiet Whether to suppress messages
#' @param preview Whether to preview after building
#' @export
build_vignettes <- function(
  pkg = ".",
  lazy = FALSE,
  quiet = FALSE,
  preview = FALSE
) {
  pkg <- as_pkgsite(pkg)

  if (!quiet) {
    cli::cli_rule("Building vignettes")
  }

  # Build individual articles
  vignettes <- get_vignettes(pkg)

  for (vignette in vignettes) {
    build_article(pkg, vignette, lazy = lazy, quiet = quiet)
  }

  if (preview) {
    preview_site(pkg, "vignettes/index.html")
  }

  invisible(pkg)
}

#' @export
#' @rdname build_vignettes
build_vignettes_index <- function(pkg = ".") {
  pkg <- as_pkgsite(pkg)

  cli::cli_inform("Writing {.file vignettes/index.html}")

  vignettes <- get_vignettes(pkg)

  if (length(vignettes) == 0) {
    article_list <- "<p>No vignettes available.</p>"
  } else {
    # Simple list of articles
    article_list <- paste0(
      "<ul>",
      paste0(
        "<li><a href=\"",
        fs::path_ext_set(vignettes, "html"),
        "\">",
        tools::file_path_sans_ext(vignettes),
        "</a></li>",
        collapse = "\n"
      ),
      "</ul>"
    )
  }

  data <- list(
    pagetitle = "Vignettes",
    contents = article_list
  )

  render_page(pkg, "article-index", data, "vignettes/index.html")
  invisible(pkg)
}

build_article <- function(pkg, name, lazy = FALSE, quiet = FALSE) {
  src_path <- fs::path(pkg$src_path, "vignettes", name)
  dst_path <- as.character(fs::path(
    "vignettes",
    fs::path_ext_set(name, "html")
  ))

  if (lazy && file.exists(fs::path(pkg$dst_path, dst_path))) {
    if (
      file.info(src_path)$mtime <=
        file.info(fs::path(pkg$dst_path, dst_path))$mtime
    ) {
      return(invisible())
    }
  }

  if (!quiet) {
    cli::cli_inform("Writing {.file {dst_path}}")
  }

  # Simple Rmd to HTML conversion
  if (fs::path_ext(src_path) %in% c("Rmd", "rmd")) {
    html_content <- simple_rmd_to_html(src_path)
  } else {
    html_content <- simple_md_to_html(src_path)
  }

  # Extract proper vignette title from VignetteIndexEntry
  article_title <- get_vignette_title(pkg, name)

  data <- list(
    pagetitle = article_title,
    title = article_title,
    contents = html_content
  )

  render_page(pkg, "article", data, dst_path, quiet = quiet)

  # Copy any generated figures to the docs directory
  copy_vignette_figures(pkg, name, quiet = quiet)

  invisible()
}

get_vignettes <- function(pkg) {
  vignette_dir <- fs::path(pkg$src_path, "vignettes")
  if (!fs::dir_exists(vignette_dir)) {
    return(character(0))
  }

  vignettes <- fs::dir_ls(vignette_dir, regexp = "\\.(Rmd|rmd|md)$")
  vignettes <- fs::path_file(vignettes)

  # Filter out files starting with _ (child documents)
  vignettes <- vignettes[!grepl("^_", vignettes)]

  return(vignettes)
}

simple_rmd_to_html <- function(path) {
  if (!file.exists(path)) {
    return("")
  }

  # Use rmarkdown to properly render Rmd to HTML
  tryCatch(
    {
      # Create a temporary output directory for rendering
      temp_dir <- tempfile()
      dir.create(temp_dir)

      # Copy the Rmd file to temp directory
      temp_rmd <- file.path(temp_dir, basename(path))
      file.copy(path, temp_rmd)

      # Create a temporary output file
      temp_html <- file.path(
        temp_dir,
        paste0(tools::file_path_sans_ext(basename(path)), ".html")
      )

      # Render the Rmd file with external figures enabled
      rmarkdown::render(
        input = temp_rmd,
        output_file = temp_html,
        output_format = rmarkdown::html_fragment(
          self_contained = FALSE
        ),
        quiet = TRUE
      )

      # Read the generated HTML content
      if (file.exists(temp_html)) {
        content <- readLines(temp_html, warn = FALSE)
        content <- paste(content, collapse = "\n")

        # Fix image paths to be relative to the vignettes directory
        content <- fix_vignette_image_paths(content)

        # Clean up temp directory
        unlink(temp_dir, recursive = TRUE)

        return(content)
      } else {
        # Clean up temp directory
        unlink(temp_dir, recursive = TRUE)
        return("")
      }
    },
    error = function(e) {
      # If rmarkdown fails, fall back to simple markdown processing
      cli::cli_warn(
        "Failed to render R Markdown file {.file {path}}: {e$message}"
      )
      simple_md_to_html(path)
    }
  )
}

simple_md_to_html <- function(path) {
  if (!file.exists(path)) {
    return("")
  }

  content <- readLines(path, warn = FALSE)
  content <- paste(content, collapse = "\n")

  # Basic markdown to HTML conversion
  content <- gsub("^# (.+)$", "<h1>\\1</h1>", content, perl = TRUE)
  content <- gsub("^## (.+)$", "<h2>\\1</h2>", content, perl = TRUE)
  content <- gsub("^### (.+)$", "<h3>\\1</h3>", content, perl = TRUE)

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

  # Convert code blocks
  content <- gsub(
    "```r\\n([^`]+)\\n```",
    "<pre><code class='language-r'>\\1</code></pre>",
    content,
    perl = TRUE
  )
  content <- gsub(
    "```\\n([^`]+)\\n```",
    "<pre><code>\\1</code></pre>",
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

  # Convert paragraphs
  lines <- strsplit(content, "\n")[[1]]
  lines <- gsub("^$", "</p><p>", lines)
  content <- paste(lines, collapse = "\n")
  content <- paste0("<p>", content, "</p>")
  content <- gsub("<p></p>", "", content, fixed = TRUE)

  return(content)
}

copy_vignette_figures <- function(pkg, vignette_name, quiet = FALSE) {
  # Look for figures generated during vignette rendering
  vignette_base <- tools::file_path_sans_ext(vignette_name)

  # Common figure directories where knitr might save figures
  possible_fig_dirs <- c(
    fs::path(pkg$src_path, "vignettes", "figures"),
    fs::path(
      pkg$src_path,
      "vignettes",
      paste0(vignette_base, "_files", "figure-html")
    ),
    fs::path(pkg$src_path, "vignettes", paste0(vignette_base, "_files")),
    fs::path(pkg$src_path, "vignettes")
  )

  # Create figures directory in docs/vignettes if it doesn't exist
  docs_fig_dir <- fs::path(pkg$dst_path, "vignettes", "figures")
  if (!fs::dir_exists(docs_fig_dir)) {
    fs::dir_create(docs_fig_dir, recurse = TRUE)
  }

  # Look for and copy figure files
  for (fig_dir in possible_fig_dirs) {
    if (fs::dir_exists(fig_dir)) {
      # Find image files (png, svg, jpg, jpeg, gif)
      image_files <- fs::dir_ls(
        fig_dir,
        regexp = "\\.(png|svg|jpg|jpeg|gif)$",
        recurse = FALSE
      )

      if (length(image_files) > 0) {
        for (img_file in image_files) {
          dst_file <- fs::path(docs_fig_dir, fs::path_file(img_file))
          fs::file_copy(img_file, dst_file, overwrite = TRUE)

          if (!quiet) {
            cli::cli_inform(
              "Copying figure {.file {fs::path_file(img_file)}}"
            )
          }
        }
      }
    }
  }
}

fix_vignette_image_paths <- function(html_content) {
  # Convert absolute paths to relative paths for images in the figures directory
  # Pattern matches: src="/absolute/path/to/vignettes/figures/filename.ext"
  # Replace with: src="figures/filename.ext"
  html_content <- gsub(
    'src="[^"]*vignettes/figures/([^"]+)"',
    'src="figures/\\1"',
    html_content,
    perl = TRUE
  )

  return(html_content)
}
