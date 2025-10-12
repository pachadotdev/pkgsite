#' Build search index
#'
#' @description
#' Creates a search index of all content for client-side searching
#'
#' @param pkg Path to package
#'
#' @importFrom jsonlite toJSON
#'
#' @examples
#' \dontrun{
#' build_search_index()
#' }
#'
#' @return Invisible `TRUE` if the search index was built successfully
#'
#' @export
build_search_index <- function(pkg = ".") {
    pkg <- as_pkgsite(pkg)

    message("Building search index")

    # Initialize search index
    search_data <- list()

    # Index reference topics
    topics <- get_reference_topics(pkg)
    if (length(topics) > 0) {
        for (topic in topics) {
            # Read the .Rd file
            rd_path <- file.path(pkg$src_path, "man", paste0(topic, ".Rd"))
            if (file.exists(rd_path)) {
                rd_content <- readLines(rd_path, warn = FALSE)

                # Extract searchable content
                content_text <- extract_searchable_content(rd_content)

                search_data[[length(search_data) + 1]] <- list(
                    title = topic,
                    url = paste0("reference/", topic, ".html"),
                    content = content_text,
                    type = "function"
                )
            }
        }
    }

    # Index vignettes
    vignette_dir <- file.path(pkg$src_path, "vignettes")
    if (dir.exists(vignette_dir)) {
        vignette_files <- list.files(
            vignette_dir,
            pattern = "\\.Rmd$",
            full.names = TRUE
        )

        for (vignette_file in vignette_files) {
            content <- readLines(vignette_file, warn = FALSE)
            # Extract title from VignetteIndexEntry
            title_line <- grep("VignetteIndexEntry", content, value = TRUE)
            if (length(title_line) > 0) {
                title <- gsub(
                    '.*VignetteIndexEntry\\{([^}]+)\\}.*',
                    '\\1',
                    title_line[1]
                )
            } else {
                title <- tools::file_path_sans_ext(basename(vignette_file))
            }

            # Extract text content (remove R chunks and YAML)
            text_content <- extract_vignette_text(content)

            filename <- tools::file_path_sans_ext(basename(vignette_file))

            search_data[[length(search_data) + 1]] <- list(
                title = title,
                url = paste0("vignettes/", filename, ".html"),
                content = text_content,
                type = "vignette"
            )
        }
    }

    # Write search index as JSON
    search_json <- jsonlite::toJSON(
        search_data,
        auto_unbox = TRUE,
        pretty = TRUE
    )
    writeLines(search_json, file.path(pkg$dst_path, "search.json"))

    invisible(TRUE)
}

extract_searchable_content <- function(rd_lines) {
    # Filter out comment lines and empty lines
    rd_lines <- rd_lines[!grepl("^%", rd_lines)]
    rd_lines <- rd_lines[rd_lines != ""]

    content <- paste(rd_lines, collapse = " ")

    # Extract content from common Rd sections and clean markup
    # Handle specific Rd sections for search content
    content <- gsub("\\\\name\\{([^}]+)\\}", "\\1", content, perl = TRUE)
    content <- gsub("\\\\alias\\{([^}]+)\\}", "", content, perl = TRUE) # Remove aliases
    content <- gsub("\\\\title\\{([^}]+)\\}", "\\1", content, perl = TRUE)
    content <- gsub("\\\\description\\{([^}]+)\\}", "\\1", content, perl = TRUE)
    content <- gsub("\\\\usage\\{([^}]+)\\}", "\\1", content, perl = TRUE)
    content <- gsub("\\\\arguments\\{([^}]+)\\}", "\\1", content, perl = TRUE)
    content <- gsub("\\\\value\\{([^}]+)\\}", "\\1", content, perl = TRUE)
    content <- gsub("\\\\details\\{([^}]+)\\}", "\\1", content, perl = TRUE)
    content <- gsub("\\\\examples\\{([^}]+)\\}", "\\1", content, perl = TRUE)

    # Handle item lists in arguments section
    content <- gsub(
        "\\\\item\\{([^}]+)\\}\\{([^}]+)\\}",
        "\\1 \\2",
        content,
        perl = TRUE
    )

    # Use shared utility for final Rd cleanup
    content <- convert_rd_to_html(content)

    # Remove HTML tags for search text
    content <- gsub("<[^>]*>", "", content)

    return(content)
}

extract_vignette_text <- function(content) {
    # Remove YAML header
    in_yaml <- FALSE
    yaml_end <- 0

    for (i in seq_along(content)) {
        if (content[i] == "---") {
            if (!in_yaml) {
                in_yaml <- TRUE
            } else {
                yaml_end <- i
                break
            }
        }
    }

    if (yaml_end > 0) {
        content <- content[(yaml_end + 1):length(content)]
    }

    # Remove R code chunks
    content <- content[!grepl("^```\\{r", content)]
    content <- content[!grepl("^```$", content)]

    # Remove markdown markup using shared utility
    text <- paste(content, collapse = " ")
    text <- clean_text_for_search(text)

    return(text)
}
