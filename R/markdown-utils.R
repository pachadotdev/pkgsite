# Markdown to HTML conversion utilities
# These functions contain common markdown processing patterns used across the package

#' Convert markdown code blocks to HTML
#'
#' This function handles both fenced code blocks with language specifications
#' and ensures proper paragraph separation.
#'
#' @param content Character string containing markdown content
#' @return Character string with code blocks converted to HTML
convert_code_blocks <- function(content) {
    # Method 1: Complex placeholder method (for cases where order matters)
    code_blocks <- list()
    block_counter <- 1

    # Find all code blocks and store them
    while (grepl("```[^\\n]*\\n", content)) {
        # Find the start of a code block
        start_match <- regexpr("```[^\\n]*\\n", content)
        if (start_match == -1) {
            break
        }

        start_pos <- start_match[1]
        start_len <- attr(start_match, "match.length")

        # Find the matching end ```
        remaining <- substr(content, start_pos + start_len, nchar(content))
        end_match <- regexpr("\\n```(\\n|$)", remaining)
        if (end_match == -1) {
            break
        }

        end_pos <- start_pos + start_len + end_match[1] - 1
        end_len <- attr(end_match, "match.length")

        # Extract the code content
        code_content <- substr(content, start_pos + start_len, end_pos)
        code_blocks[[as.character(block_counter)]] <- paste0(
            "<pre><code>",
            code_content,
            "</code></pre>"
        )

        # Replace with placeholder, ensuring it's isolated with double newlines
        placeholder <- paste0("\n\n###CODEBLOCK", block_counter, "###\n\n")
        before_part <- substr(content, 1, start_pos - 1)
        after_part <- substr(content, end_pos + end_len, nchar(content))

        # Clean up extra newlines to avoid triple/quadruple newlines
        before_part <- gsub("\\n+$", "", before_part)
        after_part <- gsub("^\\n+", "", after_part)

        content <- paste0(before_part, placeholder, after_part)

        block_counter <- block_counter + 1
    }

    # Later, after other processing, restore code blocks
    for (i in names(code_blocks)) {
        placeholder <- paste0("###CODEBLOCK", i, "###")
        content <- gsub(placeholder, code_blocks[[i]], content, fixed = TRUE)
    }

    return(content)
}

#' Convert markdown code blocks to HTML (simple regex method)
#'
#' A simpler approach using regex for cases where order doesn't matter
#'
#' @param content Character string containing markdown content
#' @param language_specific Whether to preserve language specifications
#' @return Character string with code blocks converted to HTML
convert_code_blocks_simple <- function(content, language_specific = FALSE) {
    if (language_specific) {
        # Convert R-specific code blocks first
        content <- gsub(
            "```r\\s*\\n([\\s\\S]*?)\\n```",
            "<pre><code class='language-r'>\\1</code></pre>",
            content,
            perl = TRUE
        )
        # Then convert other code blocks
        content <- gsub(
            "```\\s*\\n([\\s\\S]*?)\\n```",
            "<pre><code>\\1</code></pre>",
            content,
            perl = TRUE
        )
    } else {
        # Convert all code blocks to simple <pre><code>
        content <- gsub(
            "```[^\\n]*\\n([\\s\\S]*?)\\n```",
            "<pre><code>\\1</code></pre>",
            content,
            perl = TRUE
        )
    }

    return(content)
}

#' Convert markdown inline code to HTML
#'
#' @param content Character string containing markdown content
#' @return Character string with inline code converted to HTML
convert_inline_code <- function(content) {
    content <- gsub(
        "`([^`]+)`",
        "<code>\\1</code>",
        content,
        perl = TRUE
    )
    return(content)
}

#' Convert markdown images to HTML
#'
#' @param content Character string containing markdown content
#' @return Character string with images converted to HTML
convert_markdown_images <- function(content) {
    content <- gsub(
        "!\\[([^\\]]*)\\]\\(([^\\)]+)\\)",
        "<img src=\"\\2\" alt=\"\\1\">",
        content,
        perl = TRUE
    )
    return(content)
}

#' Convert markdown links to HTML
#'
#' @param content Character string containing markdown content
#' @return Character string with links converted to HTML
convert_markdown_links <- function(content) {
    content <- gsub(
        "\\[([^\\]]+)\\]\\(([^\\)]+)\\)",
        "<a href=\"\\2\">\\1</a>",
        content,
        perl = TRUE
    )
    return(content)
}

#' Convert angle-bracket URLs to HTML links
#'
#' @param content Character string containing markdown content
#' @return Character string with angle-bracket URLs converted to HTML
convert_angle_bracket_urls <- function(content) {
    # Convert <https://example.com> to <a href="https://example.com">https://example.com</a>
    content <- gsub(
        "<(https?://[^>]+)>",
        "<a href=\"\\1\">\\1</a>",
        content,
        perl = TRUE
    )
    return(content)
}

#' Convert markdown lists to HTML
#'
#' @param content Character string containing markdown content
#' @return Character string with lists converted to HTML
convert_markdown_lists <- function(content) {
    # Handle list processing by working with complete list blocks
    lines <- strsplit(content, "\n")[[1]]
    result <- character()
    i <- 1

    while (i <= length(lines)) {
        line <- lines[i]

        # Check if we're starting a list
        if (grepl("^\\s*[-*+]\\s+", line)) {
            # Start of unordered list - collect all consecutive list items
            list_items <- character()

            while (i <= length(lines)) {
                current_line <- lines[i]

                if (grepl("^\\s*[-*+]\\s+", current_line)) {
                    # New list item
                    item_content <- gsub(
                        "^\\s*[-*+]\\s+(.*)",
                        "\\1",
                        current_line
                    )

                    # Look ahead to collect continuation lines
                    j <- i + 1
                    while (
                        j <= length(lines) &&
                            !grepl("^\\s*[-*+]\\s+", lines[j]) &&
                            lines[j] != "" &&
                            !grepl("^\\s*$", lines[j])
                    ) {
                        # This is a continuation of the current list item
                        continuation <- trimws(lines[j])
                        if (continuation != "") {
                            item_content <- paste(item_content, continuation)
                        }
                        j <- j + 1
                    }

                    # Convert task list items [x] and [ ] to checkboxes
                    if (grepl("^\\[[ x]\\]\\s+", item_content)) {
                        if (grepl("^\\[x\\]\\s+", item_content)) {
                            # Checked item
                            item_content <- gsub(
                                "^\\[x\\]\\s+(.*)",
                                "<input type=\"checkbox\" checked disabled> \\1",
                                item_content
                            )
                        } else {
                            # Unchecked item
                            item_content <- gsub(
                                "^\\[ \\]\\s+(.*)",
                                "<input type=\"checkbox\" disabled> \\1",
                                item_content
                            )
                        }
                    }

                    list_items <- c(
                        list_items,
                        paste0("<li>", item_content, "</li>")
                    )
                    i <- j - 1
                } else if (
                    current_line == "" || grepl("^\\s*$", current_line)
                ) {
                    # Empty line ends the list
                    break
                } else {
                    # Non-list line ends the list
                    i <- i - 1
                    break
                }

                i <- i + 1
            }

            # Add the complete list to result
            result <- c(result, "<ul>", list_items, "</ul>")
        } else if (grepl("^\\s*\\d+\\.\\s+", line)) {
            # Start of ordered list - similar logic
            list_items <- character()

            while (i <= length(lines)) {
                current_line <- lines[i]

                if (grepl("^\\s*\\d+\\.\\s+", current_line)) {
                    # New list item
                    item_content <- gsub(
                        "^\\s*\\d+\\.\\s+(.*)",
                        "\\1",
                        current_line
                    )

                    # Look ahead to collect continuation lines
                    j <- i + 1
                    while (
                        j <= length(lines) &&
                            !grepl("^\\s*\\d+\\.\\s+", lines[j]) &&
                            lines[j] != "" &&
                            !grepl("^\\s*$", lines[j])
                    ) {
                        continuation <- trimws(lines[j])
                        if (continuation != "") {
                            item_content <- paste(item_content, continuation)
                        }
                        j <- j + 1
                    }

                    list_items <- c(
                        list_items,
                        paste0("<li>", item_content, "</li>")
                    )
                    i <- j - 1
                } else if (
                    current_line == "" || grepl("^\\s*$", current_line)
                ) {
                    break
                } else {
                    i <- i - 1
                    break
                }

                i <- i + 1
            }

            result <- c(result, "<ol>", list_items, "</ol>")
        } else {
            # Regular line
            result <- c(result, line)
        }

        i <- i + 1
    }

    return(paste(result, collapse = "\n"))
}

#' Convert markdown headers to HTML
#'
#' @param content Character string containing markdown content
#' @return Character string with headers converted to HTML
convert_markdown_headers <- function(content) {
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

    return(paste(html_lines, collapse = "\n"))
}

#' Convert markdown headers to HTML (simple regex method)
#'
#' A simpler approach using regex for multiline content
#'
#' @param content Character string containing markdown content
#' @return Character string with headers converted to HTML
convert_markdown_headers_simple <- function(content) {
    content <- gsub("^# (.+)$", "<h1>\\1</h1>", content, perl = TRUE)
    content <- gsub("^## (.+)$", "<h2>\\1</h2>", content, perl = TRUE)
    content <- gsub("^### (.+)$", "<h3>\\1</h3>", content, perl = TRUE)
    content <- gsub("^#### (.+)$", "<h4>\\1</h4>", content, perl = TRUE)
    return(content)
}

#' Wrap content in paragraphs
#'
#' Takes content and wraps text that isn't already HTML elements in <p> tags
#'
#' @param content Character string containing HTML content
#' @return Character string with paragraphs wrapped
wrap_paragraphs <- function(content) {
    # Split by double newlines to create paragraphs
    paragraphs <- strsplit(content, "\n\n")[[1]]
    paragraphs <- paragraphs[paragraphs != ""]

    # Filter out HTML comments and empty paragraphs
    paragraphs <- paragraphs[!grepl("^\\s*<!--.*-->\\s*$", paragraphs)]
    paragraphs <- paragraphs[paragraphs != ""]

    # Wrap non-header content in paragraphs
    for (i in seq_along(paragraphs)) {
        para <- paragraphs[i]
        # Skip if it's already a header, pre block, list, or empty
        if (
            !grepl("^<h[1-6]", para) &&
                !grepl("^<pre>", para) &&
                !grepl("^<ul>", para) &&
                !grepl("^<ol>", para) &&
                para != ""
        ) {
            paragraphs[i] <- paste0("<p>", para, "</p>")
        }
    }

    return(paste(paragraphs, collapse = "\n\n"))
}

#' Full markdown to HTML conversion
#'
#' Applies all markdown conversions in the correct order
#'
#' @param content Character string containing markdown content
#' @param use_complex_code_blocks Whether to use the complex code block method
#' @param language_specific Whether to preserve language specifications in code blocks
#' @return Character string with markdown converted to HTML
markdown_to_html_full <- function(
    content,
    use_complex_code_blocks = TRUE,
    language_specific = FALSE
) {
    # Convert images first (before links to avoid conflicts)
    content <- convert_markdown_images(content)

    # Convert links
    content <- convert_markdown_links(content)

    # Convert angle-bracket URLs
    content <- convert_angle_bracket_urls(content)

    # Convert code blocks (do this before inline code to avoid conflicts)
    if (use_complex_code_blocks) {
        content <- convert_code_blocks(content)
    } else {
        content <- convert_code_blocks_simple(content, language_specific)
    }

    # Convert inline code
    content <- convert_inline_code(content)

    # Convert lists (do this before headers to avoid conflicts)
    content <- convert_markdown_lists(content)

    # Convert headers
    if (use_complex_code_blocks) {
        # Use line-by-line method for complex processing
        content <- convert_markdown_headers(content)
    } else {
        # Use simple regex method
        content <- convert_markdown_headers_simple(content)
    }

    # Wrap in paragraphs
    content <- wrap_paragraphs(content)

    return(content)
}

# Rd/LaTeX to HTML conversion utilities

#' Clean Rd content by removing common LaTeX commands
#'
#' @param content Character string containing Rd content
#' @return Character string with LaTeX commands cleaned
clean_rd_content <- function(content) {
    # Remove name and alias commands
    content <- gsub("\\\\name\\{[^}]+\\}\\s*", "", content, perl = TRUE)
    content <- gsub("\\\\alias\\{[^}]+\\}\\s*", "", content, perl = TRUE)

    return(content)
}

#' Extract Rd section content
#'
#' @param content Character string containing Rd content
#' @param section_name Name of the section to extract (e.g., "title", "description")
#' @return Character string with section content
extract_rd_section <- function(content, section_name) {
    pattern <- paste0("\\\\", section_name, "\\{([^}]+)\\}")
    match <- regexpr(pattern, content, perl = TRUE)

    if (match != -1) {
        return(gsub(pattern, "\\1", content, perl = TRUE))
    }

    return("")
}

#' Convert common Rd commands to HTML
#'
#' @param text Character string containing Rd text
#' @return Character string with Rd commands converted to HTML
convert_rd_to_html <- function(text) {
    # Convert \code{} to HTML code tags
    text <- gsub("\\\\code\\{([^}]+)\\}", "<code>\\1</code>", text, perl = TRUE)

    # Convert \dontrun{} by keeping content
    text <- gsub("\\\\dontrun\\{([^}]*)\\}", "\\1", text, perl = TRUE)

    # Convert \link{} to HTML links (simple version)
    text <- gsub(
        "\\\\link\\{([^}]+)\\}",
        "<a href=\"#\\1\">\\1</a>",
        text,
        perl = TRUE
    )

    # Convert \link[=name]{text} to HTML links
    text <- gsub(
        "\\\\link\\[=([^\\]]+)\\]\\{([^}]+)\\}",
        "<a href=\"#\\1\">\\2</a>",
        text,
        perl = TRUE
    )

    # Handle markdown-style links [function_name()] - these come from roxygen2
    text <- gsub(
        "\\[([a-zA-Z_][a-zA-Z0-9_]*)(\\(\\))?\\]",
        "<a href=\"#\\1\">\\1\\2</a>",
        text,
        perl = TRUE
    )

    # Clean up remaining LaTeX commands by removing backslashes and braces
    text <- gsub("\\\\[a-zA-Z]+\\{([^}]*)\\}", "\\1", text, perl = TRUE)
    text <- gsub("\\\\[a-zA-Z]+", "", text, perl = TRUE)
    text <- gsub("\\{|\\}", "", text, perl = TRUE)

    # Normalize whitespace
    text <- gsub("\\s+", " ", text, perl = TRUE)
    text <- trimws(text)

    return(text)
}

#' Clean search text by removing markdown and HTML
#'
#' @param text Character string containing formatted text
#' @return Character string with formatting removed for search indexing
clean_text_for_search <- function(text) {
    # Remove headers
    text <- gsub("#+ ", "", text)

    # Remove bold and italic
    text <- gsub("\\*\\*([^*]+)\\*\\*", "\\1", text) # Bold
    text <- gsub("\\*([^*]+)\\*", "\\1", text) # Italic

    # Remove code
    text <- gsub("`([^`]+)`", "\\1", text)

    # Remove links but keep text
    text <- gsub("\\[([^]]+)\\]\\([^)]+\\)", "\\1", text)

    # Normalize whitespace
    text <- gsub("\\s+", " ", text)
    text <- trimws(text)

    return(text)
}
