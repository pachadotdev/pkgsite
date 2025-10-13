#' Build news page
#'
#' @description
#' Generates a news page from NEWS.md
#'
#' @param pkg Path to package to document.
#' @param quiet If `quiet`, will suppress output messages
#' @param preview If `TRUE`, the news page will be opened using a local server (requires the `servr` package)
#'
#' @examples
#' \dontrun{
#' build_news()
#'
#' # Hide build messages
#' build_news(quiet = TRUE)
#'
#' # Preview the news page after building
#' build_news(preview = TRUE)
#' }
#'
#' @return Invisible `TRUE` if the news page was built successfully
#'
#' @export
build_news <- function(pkg = ".", quiet = TRUE, preview = FALSE) {
    pkg <- as_pkgsite(pkg)

    if (!quiet) {
        message(paste0("-- Building news ", paste(rep("-", 64), collapse = "")))
    }

    build_news_index(pkg, quiet = quiet)

    if (preview) {
        preview_site(pkg, "news/index.html")
    }

    invisible(TRUE)
}

build_news_index <- function(pkg, quiet = FALSE) {
    news_path <- file.path(pkg$src_path, "NEWS.md")

    if (!file.exists(news_path)) {
        if (!quiet) {
            message("No NEWS.md found, skipping news page")
        }
        return(invisible())
    }

    # Create news directory
    news_dir <- file.path(pkg$dst_path, "news")
    if (!dir.exists(news_dir)) {
        dir.create(news_dir, recursive = TRUE, showWarnings = FALSE)
    }

    if (!quiet) {
        message("Reading NEWS.md")
        message("Writing news/index.html")
    }

    # Read and process the NEWS.md file
    content <- readLines(news_path, warn = FALSE)
    content <- paste(content, collapse = "\n")
    content <- markdown_to_html_full(content)

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
