#' Build news page
#'
#' @description
#' Generates a news page from NEWS.md, NEWS.Rmd, or ChangeLog files.
#'
#' @param pkg Path to package to document.
#' @param quiet If `quiet`, will suppress output messages
#' @param preview If `TRUE`, the news page will be opened in browser
#'
#' @export
build_news <- function(pkg = ".", quiet = TRUE, preview = FALSE) {
    pkg <- as_pkgsite(pkg)

    if (!quiet) {
        message(paste0("── Building news ", paste(rep("-", 64), collapse = "")))
    }

    build_news_index(pkg, quiet = quiet)

    if (preview) {
        preview_site(pkg, "news/index.html")
    }

    invisible()
}

build_news_index <- function(pkg, quiet = FALSE) {
    news_source <- find_news_source(pkg)

    if (is.null(news_source)) {
        if (!quiet) {
            message(
                "No NEWS.md, NEWS.Rmd, or ChangeLog found, skipping news page"
            )
        }
        return(invisible())
    }

    # Create news directory
    news_dir <- file.path(pkg$dst_path, "news")
    if (!dir.exists(news_dir)) {
        dir.create(news_dir, recursive = TRUE, showWarnings = FALSE)
    }

    if (!quiet) {
        message("Reading ", news_source)
        message("Writing news/index.html")
    }

    # Read and process the news file
    if (grepl("\\.(md|Rmd)$", news_source, ignore.case = TRUE)) {
        content <- readLines(news_source, warn = FALSE)
        content <- paste(content, collapse = "\n")

        # Process markdown to HTML
        if (grepl("\\.Rmd$", news_source, ignore.case = TRUE)) {
            # For .Rmd files, try to use R Markdown if available
            if (requireNamespace("rmarkdown", quietly = TRUE)) {
                tryCatch(
                    {
                        temp_file <- tempfile(fileext = ".html")
                        rmarkdown::render(
                            news_source,
                            output_file = temp_file,
                            output_format = rmarkdown::html_fragment(),
                            quiet = TRUE
                        )
                        content <- readLines(temp_file, warn = FALSE)
                        content <- paste(content, collapse = "\n")
                        unlink(temp_file)
                    },
                    error = function(e) {
                        # Fall back to simple markdown processing
                        content <- markdown_to_html_full(content)
                    }
                )
            } else {
                # Fall back to simple markdown processing
                content <- markdown_to_html_full(content)
            }
        } else {
            # For .md files, use our markdown processor
            content <- markdown_to_html_full(content)
        }
    } else {
        # For ChangeLog or other text files
        content <- readLines(news_source, warn = FALSE)
        content <- paste0("<pre>", paste(content, collapse = "\n"), "</pre>")
    }

    data <- list(
        pagetitle = "Changelog",
        title = "Changelog",
        contents = content
    )

    render_page(
        pkg,
        name = "news",
        data = data,
        path = "news/index.html",
        quiet = quiet
    )
}

find_news_source <- function(pkg) {
    candidates <- c("NEWS.md", "NEWS.Rmd", "NEWS", "ChangeLog", "CHANGES")

    for (candidate in candidates) {
        path <- file.path(pkg$src_path, candidate)
        if (file.exists(path)) {
            return(path)
        }
    }

    return(NULL)
}
