#' Build reference section
#'
#' @description
#' `build_reference()` generates reference documentation from Rd files
#'
#' @param pkg Path to package
#' @param lazy Only rebuild if source is newer than destination
#' @param preview Whether to preview after building
#' @export
build_reference <- function(
  pkg = ".",
  lazy = FALSE,
  preview = FALSE
) {
  pkg <- as_pkgsite(pkg)

  cli::cli_rule("Building function reference")

  # Build reference index
  build_reference_index(pkg)

  # Build individual reference pages
  topics <- get_reference_topics(pkg)

  for (topic in topics) {
    build_reference_topic(pkg, topic, lazy = lazy)
  }

  if (preview) {
    preview_site(pkg, "reference/index.html")
  }

  invisible(pkg)
}

#' @export
#' @rdname build_reference
build_reference_index <- function(pkg = ".") {
  pkg <- as_pkgsite(pkg)

  cli::cli_inform("Writing {.file reference/index.html}")

  topics <- get_reference_topics(pkg)

  # Simple list of functions
  topic_list <- paste0(
    "<ul>",
    paste0(
      "<li><a href=\"",
      topics,
      ".html\">",
      topics,
      "</a></li>",
      collapse = "\n"
    ),
    "</ul>"
  )

  data <- list(
    pagetitle = "Function Reference",
    contents = topic_list
  )

  render_page(pkg, "reference-index", data, "reference/index.html")
  invisible(pkg)
}

build_reference_topic <- function(pkg, topic, lazy = FALSE) {
  src_path <- fs::path(pkg$src_path, "man", paste0(topic, ".Rd"))
  dst_path <- as.character(fs::path("reference", paste0(topic, ".html")))

  if (lazy && file.exists(fs::path(pkg$dst_path, dst_path))) {
    if (
      file.info(src_path)$mtime <=
        file.info(fs::path(pkg$dst_path, dst_path))$mtime
    ) {
      return(invisible())
    }
  }

  cli::cli_inform("Writing {.file {dst_path}}")

  # Simple Rd to HTML conversion
  rd_content <- readLines(src_path, warn = FALSE)
  html_content <- simple_rd_to_html(rd_content, topic)

  data <- list(
    pagetitle = topic,
    contents = html_content
  )

  render_page(pkg, "reference-topic", data, dst_path)
  invisible()
}

get_reference_topics <- function(pkg) {
  man_files <- fs::dir_ls(fs::path(pkg$src_path, "man"), glob = "*.Rd")
  topics <- fs::path_ext_remove(fs::path_file(man_files))
  return(topics)
}

simple_rd_to_html <- function(rd_lines, topic) {
  # Remove roxygen2 comments and other unwanted lines
  rd_lines <- rd_lines[!grepl("^%", rd_lines)]
  rd_lines <- rd_lines[rd_lines != ""]

  content <- paste(rd_lines, collapse = "\n")

  # Remove \name{} and \alias{} directives that create duplicate titles
  content <- gsub("\\\\name\\{[^}]+\\}\\s*", "", content, perl = TRUE)
  content <- gsub("\\\\alias\\{[^}]+\\}\\s*", "", content, perl = TRUE)

  # Basic Rd to HTML conversion
  content <- gsub(
    "\\\\title\\{([^}]+)\\}",
    "<h1>\\1</h1>",
    content,
    perl = TRUE
  )
  # Handle description section with proper nested brace parsing
  content <- parse_and_replace_description(content)
  content <- gsub(
    "\\\\usage\\{([^}]+)\\}",
    "<h2>Usage</h2><pre class='ref-usage'>\\1</pre>",
    content,
    perl = TRUE
  )

  # Handle arguments section with proper nested brace parsing
  content <- parse_and_replace_arguments(content)

  content <- gsub(
    "\\\\value\\{([^}]+)\\}",
    "<h2>Value</h2><p>\\1</p>",
    content,
    perl = TRUE
  )
  content <- gsub(
    "\\\\details\\{([^}]+)\\}",
    "<h2>Details</h2><p>\\1</p>",
    content,
    perl = TRUE
  )
  # Handle examples section with proper nested brace parsing
  content <- parse_and_replace_examples(content)

  # Clean up any remaining Rd markup and extra whitespace
  content <- gsub("\\\\[a-zA-Z]+\\{([^}]*)\\}", "\\1", content, perl = TRUE)
  content <- gsub("\\\\[a-zA-Z]+", "", content, perl = TRUE)
  content <- gsub("\\n\\s*\\n+", "\n", content, perl = TRUE)
  content <- gsub("^\\s+|\\s+$", "", content, perl = TRUE)

  return(content)
}

parse_and_replace_description <- function(content) {
  # Find the description section using a more sophisticated approach
  if (grepl("\\\\description\\{", content)) {
    # Extract the entire description block including nested braces
    start_pos <- regexpr("\\\\description\\{", content)[1]
    if (start_pos > 0) {
      # Find the matching closing brace
      char_vec <- strsplit(content, "")[[1]]
      brace_count <- 0
      start_idx <- start_pos + nchar("\\description{") - 1
      end_idx <- start_idx

      for (i in start_idx:length(char_vec)) {
        if (char_vec[i] == "{") {
          brace_count <- brace_count + 1
        } else if (char_vec[i] == "}") {
          brace_count <- brace_count - 1
          if (brace_count == 0) {
            end_idx <- i
            break
          }
        }
      }

      if (end_idx > start_idx) {
        # Extract the description content
        desc_section <- substr(content, start_pos, end_idx)
        desc_content <- gsub(
          "\\\\description\\{(.*)\\}$",
          "\\1",
          desc_section,
          perl = TRUE
        )

        # Clean up the description content
        desc_content <- clean_rd_content(desc_content)

        # Replace the description section with cleaned HTML
        before <- substr(content, 1, start_pos - 1)
        after <- substr(content, end_idx + 1, nchar(content))
        content <- paste0(
          before,
          "<h2>Description</h2><p>",
          desc_content,
          "</p>",
          after
        )
      }
    }
  }
  return(content)
}

parse_and_replace_examples <- function(content) {
  # Find the examples section using a more sophisticated approach
  if (grepl("\\\\examples\\{", content)) {
    # Extract the entire examples block including nested braces
    start_pos <- regexpr("\\\\examples\\{", content)[1]
    if (start_pos > 0) {
      # Find the matching closing brace
      char_vec <- strsplit(content, "")[[1]]
      brace_count <- 0
      start_idx <- start_pos + nchar("\\examples{") - 1
      end_idx <- start_idx

      for (i in start_idx:length(char_vec)) {
        if (char_vec[i] == "{") {
          brace_count <- brace_count + 1
        } else if (char_vec[i] == "}") {
          brace_count <- brace_count - 1
          if (brace_count == 0) {
            end_idx <- i
            break
          }
        }
      }

      if (end_idx > start_idx) {
        # Extract the examples content
        examples_section <- substr(content, start_pos, end_idx)
        examples_content <- gsub(
          "\\\\examples\\{(.*)\\}$",
          "\\1",
          examples_section,
          perl = TRUE
        )

        # Clean up the examples content while preserving structure
        examples_content <- clean_examples_content(examples_content)

        # Replace the examples section with cleaned HTML
        before <- substr(content, 1, start_pos - 1)
        after <- substr(content, end_idx + 1, nchar(content))
        content <- paste0(
          before,
          "<h2>Examples</h2><pre><code>",
          examples_content,
          "</code></pre>",
          after
        )
      }
    }
  }
  return(content)
}

clean_examples_content <- function(text) {
  # Remove \dontrun{} wrapper silently - just extract the content inside
  text <- gsub("\\\\dontrun\\{([^}]*)\\}", "\\1", text, perl = TRUE)

  # Remove other Rd commands but keep the code structure
  text <- gsub("\\\\[a-zA-Z]+\\{([^}]*)\\}", "\\1", text, perl = TRUE)
  text <- gsub("\\\\[a-zA-Z]+", "", text, perl = TRUE)

  # Clean up any remaining braces
  text <- gsub("\\{|\\}", "", text, perl = TRUE)

  # Normalize whitespace but preserve line breaks for code readability
  text <- gsub("\\n\\s*\\n+", "\n", text, perl = TRUE) # Collapse multiple empty lines
  text <- gsub("^\\s+|\\s+$", "", text, perl = TRUE) # Trim start/end

  return(text)
}

clean_rd_content <- function(text) {
  # Convert common Rd markup to HTML or plain text
  text <- gsub(
    "\\\\link\\[=([^\\]]+)\\]\\{([^}]+)\\}",
    '<a href="../reference/\\1.html">\\2</a>',
    text,
    perl = TRUE
  ) # Convert \link[=name]{text} to clickable links
  text <- gsub(
    "\\\\link\\{([^}]+)\\}",
    '<a href="../reference/\\1.html">\\1</a>',
    text,
    perl = TRUE
  ) # Convert \link{} to clickable links

  # Handle markdown-style links [function_name()] - these come from roxygen2
  text <- gsub(
    "\\[([a-zA-Z_][a-zA-Z0-9_]*)(\\(\\))?\\]",
    '<a href="../reference/\\1.html"><code>\\1\\2</code></a>',
    text,
    perl = TRUE
  )

  text <- gsub("\\\\code\\{([^}]+)\\}", "<code>\\1</code>", text, perl = TRUE) # Convert \code{}
  text <- gsub(
    "\\\\strong\\{([^}]+)\\}",
    "<strong>\\1</strong>",
    text,
    perl = TRUE
  ) # Convert \strong{}
  text <- gsub("\\\\emph\\{([^}]+)\\}", "<em>\\1</em>", text, perl = TRUE) # Convert \emph{}

  # Convert itemize/enumerate lists to HTML lists
  text <- gsub("\\\\itemize\\{([^}]*)\\}", "<ul>\\1</ul>", text, perl = TRUE)
  text <- gsub("\\\\item\\s+", "<li>", text, perl = TRUE)
  text <- gsub("</li>\\s*<li>", "</li><li>", text, perl = TRUE) # Clean up adjacent list items
  text <- gsub("<li>([^<]*)</ul>", "<li>\\1</li></ul>", text, perl = TRUE) # Close last list item

  # Remove any remaining unhandled Rd commands
  text <- gsub("\\\\[a-zA-Z]+\\{([^}]*)\\}", "\\1", text, perl = TRUE)
  text <- gsub("\\\\[a-zA-Z]+", "", text, perl = TRUE)

  # Clean up structural patterns
  text <- gsub("\\{\\s*\\{", "", text, perl = TRUE) # Remove {{ patterns
  text <- gsub("\\}\\s*\\}", "", text, perl = TRUE) # Remove }} patterns
  text <- gsub("\\{|\\}", "", text, perl = TRUE) # Remove remaining single braces

  # Clean up whitespace but preserve some structure for lists
  text <- gsub("\\n\\s*\\n+", "\n", text, perl = TRUE) # Collapse multiple newlines
  text <- gsub("\\s+", " ", text, perl = TRUE) # Normalize other whitespace
  text <- gsub("^\\s+|\\s+$", "", text, perl = TRUE) # Trim

  return(text)
}

parse_and_replace_arguments <- function(content) {
  # Find the arguments section using a more sophisticated approach
  if (grepl("\\\\arguments\\{", content)) {
    # Extract the entire arguments block including nested braces
    start_pos <- regexpr("\\\\arguments\\{", content)[1]
    if (start_pos > 0) {
      # Find the matching closing brace
      char_vec <- strsplit(content, "")[[1]]
      brace_count <- 0
      start_idx <- start_pos + nchar("\\arguments{") - 1
      end_idx <- start_idx

      for (i in start_idx:length(char_vec)) {
        if (char_vec[i] == "{") {
          brace_count <- brace_count + 1
        } else if (char_vec[i] == "}") {
          brace_count <- brace_count - 1
          if (brace_count == 0) {
            end_idx <- i
            break
          }
        }
      }

      if (end_idx > start_idx) {
        # Extract the arguments content
        args_section <- substr(content, start_pos, end_idx)
        args_content <- gsub(
          "\\\\arguments\\{(.*)\\}$",
          "\\1",
          args_section,
          perl = TRUE
        )

        # Parse the arguments and create table
        args_table <- parse_arguments_to_table(args_content)

        # Replace the arguments section with the table
        before <- substr(content, 1, start_pos - 1)
        after <- substr(content, end_idx + 1, nchar(content))
        content <- paste0(before, args_table, after)
      }
    }
  }
  return(content)
}

parse_arguments_to_table <- function(args_content) {
  # Clean up the arguments content and extract \item{name}{description} patterns
  args_content <- gsub("\\n", " ", args_content, perl = TRUE) # collapse newlines
  args_content <- gsub("\\s+", " ", args_content, perl = TRUE) # normalize whitespace

  # Find all \item{name}{description} patterns - handle nested braces properly
  # First, let's manually parse this step by step
  items <- list()

  # Find all \item positions
  item_positions <- gregexpr("\\\\item\\{", args_content, perl = TRUE)[[1]]

  if (length(item_positions) == 0 || item_positions[1] == -1) {
    return("<h2>Arguments</h2><p>No arguments.</p>")
  }

  for (i in seq_along(item_positions)) {
    start_pos <- item_positions[i] + nchar("\\item{") - 1

    # Find the argument name (everything until the first })
    name_end <- regexpr(
      "\\}",
      substr(args_content, start_pos, nchar(args_content))
    )[1]
    if (name_end == -1) {
      next
    }

    arg_name <- substr(args_content, start_pos + 1, start_pos + name_end - 2)

    # Find the description start (after }{)
    desc_start <- start_pos + name_end + 1

    # Find where this item ends (next \item or end of arguments)
    if (i < length(item_positions)) {
      desc_end <- item_positions[i + 1] - 1
    } else {
      # Last item - find the closing } of arguments
      desc_end <- nchar(args_content) - 1 # Remove the final }
    }

    arg_desc <- substr(args_content, desc_start, desc_end)
    # Remove trailing } and whitespace
    arg_desc <- gsub("\\}\\s*$", "", arg_desc)
    arg_desc <- trimws(arg_desc)

    if (arg_name != "" && arg_desc != "") {
      items[[length(items) + 1]] <- list(name = arg_name, desc = arg_desc)
    }
  }

  if (length(items) == 0) {
    return("<h2>Arguments</h2><p>No arguments.</p>")
  }

  table_rows <- ""

  for (item in items) {
    arg_name <- item$name
    arg_desc <- item$desc # Clean up and convert inline code
    arg_desc <- gsub(
      "\\\\code\\{([^}]+)\\}",
      "<code>\\1</code>",
      arg_desc,
      perl = TRUE
    )

    table_rows <- paste0(
      table_rows,
      "<tr><td><strong>",
      arg_name,
      "</strong></td><td>",
      arg_desc,
      "</td></tr>\n"
    )
  }

  if (table_rows == "") {
    return("<h2>Arguments</h2><p>No arguments.</p>")
  }

  return(paste0(
    "<h2>Arguments</h2><table class='ref-arguments'>",
    table_rows,
    "</table>"
  ))
}
