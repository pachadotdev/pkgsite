#' Build reference section
#'
#' @description
#' `build_reference()` generates reference documentation from Rd files using tools::Rd2HTML
#'
#' @param pkg Path to package
#' @param lazy Only rebuild if source is newer than destination
#' @param preview Whether to preview after building using a local server (requires the `servr` package)
#'
#' @examples
#' \dontrun{
#' build_reference()
#'
#' # Only rebuild if source is newer than destination
#' build_reference(lazy = TRUE)
#'
#' # Preview the site after building
#' build_reference(preview = TRUE)
#' }
#'
#' @return Invisible `TRUE` if the reference was built successfully
#'
#' @export
build_reference <- function(
  pkg = ".",
  lazy = FALSE,
  preview = FALSE
) {
  pkg <- as_pkgsite(pkg)

  message(paste0(
    "-- Building function reference ",
    paste(rep("-", 50), collapse = "")
  ))

  # Read NAMESPACE to get exported functions
  namespace_file <- file.path(pkg$src_path, "NAMESPACE")
  if (file.exists(namespace_file)) {
    namespace_lines <- readLines(namespace_file)
    export_lines <- grep("^export\\(", namespace_lines, value = TRUE)
    exported_topics <- gsub("export\\((.*)\\)", "\\1", export_lines)
  } else {
    # If no NAMESPACE file, build all
    exported_topics <- NULL
  }

  # Build individual reference pages from Rd files
  rd_files <- list.files(
    file.path(pkg$src_path, "man"),
    pattern = "\\.Rd$",
    full.names = TRUE
  )

  for (rd_file in rd_files) {
    topic <- tools::file_path_sans_ext(basename(rd_file))

    # Skip if not exported (and we have a NAMESPACE file)
    if (!is.null(exported_topics) && !topic %in% exported_topics) {
      next
    }

    build_reference_topic(pkg, topic, rd_file, lazy = lazy)
  }

  if (preview) {
    # Preview first exported function if available
    if (!is.null(exported_topics) && length(exported_topics) > 0) {
      preview_site(pkg, paste0("reference/", exported_topics[1], ".html"))
    }
  }

  invisible(TRUE)
}

build_reference_topic <- function(pkg, topic, rd_file, lazy = FALSE) {
  dst_path <- file.path("reference", paste0(topic, ".html"))

  if (lazy && file.exists(file.path(pkg$dst_path, dst_path))) {
    if (
      file.info(rd_file)$mtime <=
        file.info(file.path(pkg$dst_path, dst_path))$mtime
    ) {
      return(invisible())
    }
  }

  message("Writing ", dst_path)

  # Use tools::Rd2HTML to convert Rd file to HTML
  temp_html <- tempfile(fileext = ".html")

  # Convert Rd to full HTML (not fragment) for better formatting
  tools::Rd2HTML(
    rd_file,
    out = temp_html,
    package = pkg$package,
    stages = c("install", "render")
  )

  # Read the generated HTML
  html_lines <- readLines(temp_html, warn = FALSE)

  # Extract content between <body> and </body>
  body_start <- which(grepl("<body>", html_lines, fixed = TRUE))
  body_end <- which(grepl("</body>", html_lines, fixed = TRUE))

  if (length(body_start) > 0 && length(body_end) > 0) {
    # Get content between body tags
    content_lines <- html_lines[(body_start[1] + 1):(body_end[1] - 1)]
    html_content <- paste(content_lines, collapse = "\n")
  } else {
    # Fallback: use entire content
    html_content <- paste(html_lines, collapse = "\n")
  }

  # Clean up HTML: Remove <p> tags inside table cells for better alignment
  # This prevents vertical misalignment in argument tables
  html_content <- gsub("<td>\n<p>", "<td>\n", html_content, fixed = TRUE)
  html_content <- gsub("<td><p>", "<td>", html_content, fixed = TRUE)
  html_content <- gsub("</p>\n</td>", "\n</td>", html_content, fixed = TRUE)
  html_content <- gsub("</p></td>", "</td>", html_content, fixed = TRUE)

  # Remove the R documentation footer that includes package version
  # Split by lines to find and remove the exact line
  lines <- strsplit(html_content, "\n")[[1]]

  # Find lines containing the package footer pattern
  footer_pattern <- "\\[Package.*version.*\\]"
  lines_to_keep <- c()

  for (i in seq_along(lines)) {
    # Check if this line has the package footer
    if (grepl(footer_pattern, lines[i])) {
      # Skip this line and check if previous line was <hr>
      if (
        length(lines_to_keep) > 0 &&
          grepl("^<hr>\\s*$", lines_to_keep[length(lines_to_keep)])
      ) {
        # Remove the <hr> too
        lines_to_keep <- lines_to_keep[-length(lines_to_keep)]
      }
      next
    }
    lines_to_keep <- c(lines_to_keep, lines[i])
  }

  html_content <- paste(lines_to_keep, collapse = "\n")

  # Clean up temporary file
  unlink(temp_html)

  data <- list(
    pagetitle = topic,
    contents = html_content
  )

  render_page(pkg, "reference-topic", data, dst_path)
  invisible()
}
