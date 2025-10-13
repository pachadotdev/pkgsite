#' Build reference section
#'
#' @description
#' `build_reference()` generates reference documentation from Roxygen comments
#'
#' @param pkg Path to package
#' @param lazy Only rebuild if source is newer than destination
#' @param preview Whether to preview after building
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

  invisible(TRUE)
}

#' @rdname build_reference
build_reference_index <- function(pkg = ".") {
  pkg <- as_pkgsite(pkg)

  message("Writing reference/index.html")

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
  dst_path <- file.path("reference", paste0(topic, ".html"))

  # Find the R source file for this topic
  r_files <- list.files(
    file.path(pkg$src_path, "R"),
    pattern = "\\.R$",
    full.names = TRUE
  )

  topic_info <- NULL
  for (r_file in r_files) {
    topic_info <- extract_roxygen_for_topic(r_file, topic)
    if (!is.null(topic_info)) break
  }

  if (is.null(topic_info)) {
    warning("Could not find roxygen documentation for topic: ", topic)
    return(invisible())
  }

  if (lazy && file.exists(file.path(pkg$dst_path, dst_path))) {
    if (
      file.info(topic_info$source_file)$mtime <=
        file.info(file.path(pkg$dst_path, dst_path))$mtime
    ) {
      return(invisible())
    }
  }

  message("Writing ", dst_path)

  # Convert roxygen to HTML
  html_content <- roxygen_to_html(topic_info)

  data <- list(
    pagetitle = topic,
    contents = html_content
  )

  render_page(pkg, "reference-topic", data, dst_path)
  invisible()
}

extract_roxygen_for_topic <- function(r_file, topic) {
  lines <- readLines(r_file, warn = FALSE)

  # Find the function definition
  func_pattern <- paste0("^", topic, "\\s*<-\\s*function")
  func_line <- which(grepl(func_pattern, lines))

  if (length(func_line) == 0) {
    return(NULL)
  }

  # Look backwards from the function definition to find roxygen comments
  roxygen_lines <- c()
  for (i in (func_line[1] - 1):1) {
    if (grepl("^#'", lines[i])) {
      roxygen_lines <- c(lines[i], roxygen_lines)
    } else if (grepl("^\\s*$", lines[i])) {
      # Skip empty lines
      next
    } else {
      # Hit non-roxygen, non-empty line
      break
    }
  }

  if (length(roxygen_lines) == 0) {
    return(NULL)
  }

  # Check if the function is exported (has @export tag)
  has_export <- any(grepl("^#'\\s*@export", roxygen_lines))

  if (!has_export) {
    return(NULL) # Skip non-exported functions
  }

  return(list(
    topic = topic,
    source_file = r_file,
    roxygen_lines = roxygen_lines
  ))
}

roxygen_to_html <- function(topic_info) {
  lines <- topic_info$roxygen_lines

  # Remove #' prefix and process
  content_lines <- gsub("^#'\\s?", "", lines)

  # Parse roxygen sections
  title <- ""
  description <- ""
  params <- list()
  examples <- ""
  current_section <- "title"
  current_content <- c()

  for (line in content_lines) {
    if (grepl("^@description", line)) {
      if (current_section == "title") {
        title <- paste(current_content, collapse = " ")
      }
      current_section <- "description"
      current_content <- c()
    } else if (grepl("^@param", line)) {
      if (current_section == "description") {
        description <- paste(current_content, collapse = "\n")
      }
      # Extract param name and description
      param_match <- regexpr("^@param\\s+(\\S+)\\s+(.*)", line, perl = TRUE)
      if (param_match[1] > 0) {
        param_name <- sub("^@param\\s+(\\S+)\\s+.*", "\\1", line, perl = TRUE)
        param_desc <- sub("^@param\\s+\\S+\\s+(.*)", "\\1", line, perl = TRUE)
        params[[param_name]] <- param_desc
      }
    } else if (grepl("^@examples", line)) {
      if (current_section == "description") {
        description <- paste(current_content, collapse = "\n")
      }
      current_section <- "examples"
      current_content <- c()
    } else if (grepl("^@", line)) {
      # Other roxygen tags, skip for now
      next
    } else {
      current_content <- c(current_content, line)
    }
  }

  # Handle the last section
  if (current_section == "title" && length(current_content) > 0) {
    title <- paste(current_content, collapse = " ")
  } else if (current_section == "description" && length(current_content) > 0) {
    description <- paste(current_content, collapse = "\n")
  } else if (current_section == "examples" && length(current_content) > 0) {
    examples <- paste(current_content, collapse = "\n")
  }

  # Convert to HTML
  html <- paste0("<h1>", title, "</h1>\n")

  if (nchar(description) > 0) {
    # Process description as markdown
    desc_html <- markdown_to_html_simple(description)
    html <- paste0(html, "<h2>Description</h2>\n", desc_html, "\n")
  }

  if (length(params) > 0) {
    html <- paste0(
      html,
      "<h2>Arguments</h2>\n<table class=\"ref-arguments\">\n<tbody>\n"
    )
    for (param_name in names(params)) {
      param_desc_html <- markdown_to_html_inline(params[[param_name]])
      html <- paste0(
        html,
        "<tr><td><strong>",
        param_name,
        "</strong></td><td>",
        param_desc_html,
        "</td></tr>\n"
      )
    }
    html <- paste0(html, "</tbody>\n</table>\n")
  }

  if (nchar(examples) > 0) {
    # Check if examples contain \dontrun{}
    has_dontrun <- grepl("\\\\dontrun\\{", examples, perl = TRUE)

    if (has_dontrun) {
      # Just show code without execution if dontrun is present
      examples_clean <- gsub(
        "\\\\dontrun\\{([\\s\\S]*?)\\}",
        "\\1",
        examples,
        perl = TRUE
      )
      examples_clean <- gsub("^\\s+|\\s+$", "", examples_clean, perl = TRUE)
      html <- paste0(
        html,
        "<h2>Examples</h2>\n<pre><code>",
        examples_clean,
        "</code></pre>\n"
      )
    } else {
      # Execute examples and show both code and output
      examples_html <- execute_and_format_examples(examples)
      html <- paste0(html, "<h2>Examples</h2>\n", examples_html)
    }
  }

  return(html)
}

markdown_to_html_simple <- function(text) {
  # Convert markdown-style links [function_name()] to HTML links
  text <- gsub(
    "\\[([a-zA-Z_][a-zA-Z0-9_]*)(\\(\\))?\\]",
    '<a href="../reference/\\1.html"><code>\\1\\2</code></a>',
    text,
    perl = TRUE
  )

  # Convert `code` to <code>code</code>
  text <- gsub("`([^`]+)`", "<code>\\1</code>", text, perl = TRUE)

  # Convert * list items to HTML lists
  if (grepl("\\n\\s*\\*\\s+", text, perl = TRUE)) {
    lines <- strsplit(text, "\n")[[1]]
    list_lines <- c()
    in_list <- FALSE

    for (line in lines) {
      if (grepl("^\\s*\\*\\s+", line)) {
        if (!in_list) {
          list_lines <- c(list_lines, "<ul>")
          in_list <- TRUE
        }
        item_text <- sub("^\\s*\\*\\s+", "", line)
        # Process basic markdown in list items but don't recursively call
        item_text <- gsub(
          "`([^`]+)`",
          "<code>\\1</code>",
          item_text,
          perl = TRUE
        )
        item_text <- gsub(
          "\\[([a-zA-Z_][a-zA-Z0-9_]*)(\\(\\))?\\]",
          '<a href="../reference/\\1.html"><code>\\1\\2</code></a>',
          item_text,
          perl = TRUE
        )
        list_lines <- c(list_lines, paste0("<li>", item_text, "</li>"))
      } else if (nchar(trimws(line)) == 0 && in_list) {
        # Empty line in list, continue
        next
      } else {
        if (in_list) {
          list_lines <- c(list_lines, "</ul>")
          in_list <- FALSE
        }
        if (nchar(trimws(line)) > 0) {
          list_lines <- c(list_lines, paste0("<p>", line, "</p>"))
        }
      }
    }

    if (in_list) {
      list_lines <- c(list_lines, "</ul>")
    }

    text <- paste(list_lines, collapse = "\n")
  } else {
    # Not a list, wrap in paragraph
    text <- paste0("<p>", text, "</p>")
  }

  return(text)
}

markdown_to_html_inline <- function(text) {
  # Same as markdown_to_html_simple but without wrapping in <p> tags
  # Convert markdown-style links [function_name()] to HTML links
  text <- gsub(
    "\\[([a-zA-Z_][a-zA-Z0-9_]*)(\\(\\))?\\]",
    '<a href="../reference/\\1.html"><code>\\1\\2</code></a>',
    text,
    perl = TRUE
  )

  # Convert `code` to <code>code</code>
  text <- gsub("`([^`]+)`", "<code>\\1</code>", text, perl = TRUE)

  # Handle simple inline lists (but don't convert to full HTML lists)
  # Just clean up any excessive whitespace
  text <- gsub("\\s+", " ", text, perl = TRUE)
  text <- trimws(text)

  return(text)
}

simple_rd_to_html <- function(rd_lines, topic) {
  # Remove roxygen2 comments but preserve empty lines
  rd_lines <- rd_lines[!grepl("^%", rd_lines)]
  # Don't remove empty lines - they might be important for formatting (especially in examples)

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
  # Don't collapse newlines inside <pre><code> blocks (examples)
  content <- preserve_code_blocks_whitespace(content)
  content <- gsub("^\\s+|\\s+$", "", content, perl = TRUE)

  return(content)
}

parse_and_replace_description <- function(content) {
  cat(
    "DEBUG: parse_and_replace_description called\n",
    file = "/tmp/debug.log",
    append = TRUE
  )

  # Find the description section using a more sophisticated approach
  if (grepl("\\\\description\\{", content)) {
    cat(
      "DEBUG: Found description section\n",
      file = "/tmp/debug.log",
      append = TRUE
    )
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

      cat(
        "DEBUG: end_idx=",
        end_idx,
        " start_idx=",
        start_idx,
        "\n",
        file = "/tmp/debug.log",
        append = TRUE
      )

      if (end_idx > start_idx) {
        cat(
          "DEBUG: Extracting description content\n",
          file = "/tmp/debug.log",
          append = TRUE
        )

        # Extract the description content
        desc_section <- substr(content, start_pos, end_idx)
        # Extract content between the outermost braces by removing the wrapper
        desc_content <- substr(
          desc_section,
          nchar("\\description{") + 1,
          nchar(desc_section) - 1
        )

        cat(
          "DEBUG: About to call clean_rd_content\n",
          file = "/tmp/debug.log",
          append = TRUE
        )

        # Clean up the description content
        desc_content <- clean_rd_content(desc_content)

        cat(
          "DEBUG: clean_rd_content returned\n",
          file = "/tmp/debug.log",
          append = TRUE
        )

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
        # Use a more careful extraction that preserves newlines
        examples_content <- gsub(
          "^\\\\examples\\{([\\s\\S]*)\\}$",
          "\\1",
          examples_section,
          perl = TRUE
        )

        # Clean up the examples content while preserving structure
        examples_content <- clean_examples_content(examples_content) # Replace the examples section with cleaned HTML
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

preserve_code_blocks_whitespace <- function(content) {
  # Preserve whitespace inside <pre><code>...</code></pre> blocks
  # First, extract all code blocks
  code_blocks <- list()
  block_counter <- 1

  while (grepl("<pre><code>[\\s\\S]*?</code></pre>", content, perl = TRUE)) {
    match <- regexpr("<pre><code>[\\s\\S]*?</code></pre>", content, perl = TRUE)
    if (match[1] == -1) {
      break
    }

    # Extract the matched code block
    start_pos <- match[1]
    end_pos <- start_pos + attr(match, "match.length") - 1
    code_block <- substr(content, start_pos, end_pos)

    # Store it with a placeholder
    placeholder <- paste0("###CODEBLOCK", block_counter, "###")
    code_blocks[[placeholder]] <- code_block

    # Replace in content with placeholder
    before_part <- substr(content, 1, start_pos - 1)
    after_part <- substr(content, end_pos + 1, nchar(content))
    content <- paste0(before_part, placeholder, after_part)

    block_counter <- block_counter + 1
  }

  # Now collapse newlines in the non-code content
  content <- gsub("\\n\\s*\\n+", "\n", content, perl = TRUE)

  # Restore code blocks with original whitespace
  for (placeholder in names(code_blocks)) {
    content <- gsub(
      placeholder,
      code_blocks[[placeholder]],
      content,
      fixed = TRUE
    )
  }

  return(content)
}

#' @importFrom utils capture.output
execute_and_format_examples <- function(examples) {
  # Clean up the examples text first
  examples_clean <- gsub("^\\s+|\\s+$", "", examples, perl = TRUE)

  # Split into individual expressions
  # This is a simple approach - split by empty lines or by complete statements
  lines <- strsplit(examples_clean, "\n")[[1]]

  # Group lines into expressions (simple heuristic)
  expressions <- c()
  current_expr <- c()

  for (line in lines) {
    line <- trimws(line)
    if (line == "") {
      if (length(current_expr) > 0) {
        expressions <- c(expressions, paste(current_expr, collapse = "\n"))
        current_expr <- c()
      }
    } else {
      current_expr <- c(current_expr, line)
    }
  }

  # Add the last expression if any
  if (length(current_expr) > 0) {
    expressions <- c(expressions, paste(current_expr, collapse = "\n"))
  }

  # Execute each expression and format the output
  html_output <- c()

  for (expr in expressions) {
    if (nchar(trimws(expr)) == 0) {
      next
    }

    # Show the code
    html_output <- c(
      html_output,
      paste0('<div class="sourceCode"><pre><code>', expr, '</code></pre></div>')
    )

    # Try to execute and capture output
    tryCatch(
      {
        # Capture both output and any printed results
        output <- capture.output({
          result <- eval(parse(text = expr))
          if (!is.null(result) && !identical(result, invisible())) {
            print(result)
          }
        })

        if (length(output) > 0) {
          output_text <- paste(output, collapse = "\n")
          html_output <- c(
            html_output,
            paste0(
              '<div class="output"><pre><code>## ',
              gsub("\n", "\n## ", output_text),
              '</code></pre></div>'
            )
          )
        }
      },
      error = function(e) {
        # If there's an error, show it
        html_output <<- c(
          html_output,
          paste0(
            '<div class="error"><pre><code>## Error: ',
            e$message,
            '</code></pre></div>'
          )
        )
      }
    )
  }

  return(paste(html_output, collapse = "\n"))
}

clean_examples_content <- function(text) {
  # Remove \dontrun{} wrapper silently - just extract the content inside
  # Use a multiline-aware regex and trim the extracted content
  text <- gsub("\\\\dontrun\\{([\\s\\S]*?)\\}", "\\1", text, perl = TRUE)

  # After extracting dontrun content, trim leading/trailing whitespace
  text <- gsub("^\\s+|\\s+$", "", text, perl = TRUE)

  # Remove other Rd commands but keep the code structure
  text <- gsub("\\\\[a-zA-Z]+\\{([\\s\\S]*?)\\}", "\\1", text, perl = TRUE)
  text <- gsub("\\\\[a-zA-Z]+", "", text, perl = TRUE)

  # Clean up any remaining braces
  text <- gsub("\\{|\\}", "", text, perl = TRUE)

  return(text)
}

clean_rd_content <- function(text) {
  cat(
    "DEBUG: clean_rd_content called!\n",
    file = "/tmp/debug.log",
    append = TRUE
  )

  # Just return the text for now to test if the function is working
  return(text)

  message(
    "DEBUG: Starting clean_rd_content with: ",
    substr(text, 1, 100),
    "..."
  )

  # Process nested structures first - handle \code{\link[=name]{text}} patterns
  # This needs to be done before individual \code{} and \link{} processing
  tryCatch(
    {
      text <- gsub(
        "\\\\code\\{\\\\link\\[=([^\\]]+)\\]\\{([^}]+)\\}\\}",
        '<code><a href="../reference/\\1.html">\\2</a></code>',
        text,
        perl = TRUE
      )
      message(
        "DEBUG: After nested code+link processing: ",
        substr(text, 1, 100),
        "..."
      )
    },
    error = function(e) {
      message("ERROR in nested code+link gsub: ", e$message)
      # Continue with original text if there's an error
    }
  )

  # Convert \link[=name]{text} to clickable links
  tryCatch(
    {
      text <- gsub(
        "\\\\link\\[=([^\\]]+)\\]\\{([^}]+)\\}",
        '<a href="../reference/\\1.html">\\2</a>',
        text,
        perl = TRUE
      )
      message("DEBUG: After link processing")
    },
    error = function(e) {
      message("ERROR in link gsub: ", e$message)
    }
  )

  # Convert \link{} to clickable links
  text <- gsub(
    "\\\\link\\{([^}]+)\\}",
    '<a href="../reference/\\1.html">\\1</a>',
    text,
    perl = TRUE
  )

  # Handle markdown-style links [function_name()] - these come from roxygen2
  text <- gsub(
    "\\[([a-zA-Z_][a-zA-Z0-9_]*)(\\(\\))?\\]",
    '<a href="../reference/\\1.html"><code>\\1\\2</code></a>',
    text,
    perl = TRUE
  )

  # Convert \code{} to HTML code tags
  text <- gsub("\\\\code\\{([^}]+)\\}", "<code>\\1</code>", text, perl = TRUE)

  # Convert other formatting
  text <- gsub(
    "\\\\strong\\{([^}]+)\\}",
    "<strong>\\1</strong>",
    text,
    perl = TRUE
  )
  text <- gsub("\\\\emph\\{([^}]+)\\}", "<em>\\1</em>", text, perl = TRUE)

  # Handle itemize lists with a more systematic approach
  if (grepl("\\\\itemize\\{", text)) {
    # Replace \itemize{ with <ul> and mark where it ends
    text <- gsub("\\\\itemize\\{", "ITEMIZE_START<ul>", text, perl = TRUE)

    # Replace \item with <li>
    text <- gsub("\\\\item\\s+", "<li>", text, perl = TRUE)

    # Now we need to find the closing } for the original \itemize and replace it with </ul>
    # This is tricky with regex, so let's use a simple heuristic:
    # Since we know the structure should be clean from roxygen2, we can assume
    # the } that closes the itemize comes after all the list items

    # First, let's close any open <li> tags before we close the list
    # This regex looks for <li> followed by content, then either another <li> or the end
    text <- gsub(
      "<li>([^<]*?)(?=<li>|ITEMIZE_END|$)",
      "<li>\\1</li>",
      text,
      perl = TRUE
    )

    # Now find the } that should close the itemize and replace with </ul>
    # We'll look for the first } after our ITEMIZE_START marker
    text <- gsub(
      "ITEMIZE_START(<ul>.*?)\\}",
      "\\1</li></ul>",
      text,
      perl = TRUE
    )
  }

  # Clean up any remaining unhandled Rd commands (but be more careful)
  text <- gsub("\\\\dontrun\\{([\\s\\S]*?)\\}", "\\1", text, perl = TRUE) # Remove \dontrun but keep content
  text <- gsub("\\\\[a-zA-Z]+\\{([^}]*)\\}", "\\1", text, perl = TRUE) # Remove other commands
  text <- gsub("\\\\[a-zA-Z]+", "", text, perl = TRUE) # Remove command names without braces

  # Clean up structural patterns
  text <- gsub("\\{\\s*\\{", "", text, perl = TRUE) # Remove {{ patterns
  text <- gsub("\\}\\s*\\}", "", text, perl = TRUE) # Remove }} patterns
  # Remove remaining single braces (the problematic regex is replaced with this simpler one)
  text <- gsub("\\{|\\}", "", text, perl = TRUE)

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
