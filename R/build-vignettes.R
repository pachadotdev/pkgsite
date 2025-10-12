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
    message(paste0(
      "-- Building vignettes ",
      paste(rep("-", 60), collapse = "")
    ))
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

  message("Writing vignettes/index.html")

  vignettes <- get_vignettes(pkg)

  if (length(vignettes) == 0) {
    article_list <- "<p>No vignettes available.</p>"
  } else {
    # Simple list of articles
    article_list <- paste0(
      "<ul>",
      paste0(
        "<li><a href=\"",
        paste0(tools::file_path_sans_ext(vignettes), ".html"),
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
  src_path <- file.path(pkg$src_path, "vignettes", name)
  dst_path <- file.path(
    "vignettes",
    paste0(tools::file_path_sans_ext(name), ".html")
  )

  if (lazy && file.exists(file.path(pkg$dst_path, dst_path))) {
    if (
      file.info(src_path)$mtime <=
        file.info(file.path(pkg$dst_path, dst_path))$mtime
    ) {
      return(invisible())
    }
  }

  if (!quiet) {
    message("Writing ", dst_path)
  }

  # Simple Rmd to HTML conversion
  if (tools::file_ext(src_path) %in% c("Rmd", "rmd")) {
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

      # Copy all auxiliary files from the vignettes directory (including data files)
      vignette_dir <- dirname(path)
      aux_files <- list.files(
        vignette_dir,
        pattern = "\\.(bib|csl|css|js|rds|RDS|csv|txt|json|xml|yaml|yml)$",
        full.names = TRUE
      )
      if (length(aux_files) > 0) {
        for (aux_file in aux_files) {
          file.copy(aux_file, file.path(temp_dir, basename(aux_file)))
        }
      }

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
      warning(
        "Failed to render R Markdown file ",
        path,
        ": ",
        e$message,
        call. = FALSE
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

  # Use the shared markdown utilities with language-specific code blocks
  content <- markdown_to_html_full(
    content,
    use_complex_code_blocks = FALSE,
    language_specific = TRUE
  )

  # The old version used a different paragraph wrapping method,
  # but the new utility method should work better
  return(content)
}

copy_vignette_figures <- function(pkg, vignette_name, quiet = FALSE) {
  # Look for figures generated during vignette rendering
  vignette_base <- tools::file_path_sans_ext(vignette_name)

  # Common figure directories where knitr might save figures
  possible_fig_dirs <- c(
    file.path(pkg$src_path, "vignettes", "figures"),
    file.path(
      pkg$src_path,
      "vignettes",
      paste0(vignette_base, "_files", "figure-html")
    ),
    file.path(pkg$src_path, "vignettes", paste0(vignette_base, "_files")),
    file.path(pkg$src_path, "vignettes")
  )

  # Create figures directory in docs/vignettes if it doesn't exist
  docs_fig_dir <- file.path(pkg$dst_path, "vignettes", "figures")
  if (!dir.exists(docs_fig_dir)) {
    dir.create(docs_fig_dir, recursive = TRUE)
  }

  # Look for and copy figure files
  for (fig_dir in possible_fig_dirs) {
    if (dir.exists(fig_dir)) {
      # Find image files (png, svg, jpg, jpeg, gif)
      image_files <- list.files(
        fig_dir,
        pattern = "\\.(png|svg|jpg|jpeg|gif)$",
        recursive = FALSE,
        full.names = TRUE
      )

      if (length(image_files) > 0) {
        for (img_file in image_files) {
          dst_file <- file.path(docs_fig_dir, basename(img_file))
          file.copy(img_file, dst_file, overwrite = TRUE)

          if (!quiet) {
            message("Copying figure ", basename(img_file))
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
