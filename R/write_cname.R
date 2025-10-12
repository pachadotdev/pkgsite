write_cname <- function(url, path) {
    # remove http:// or https:// if present
    url <- gsub("^https?://", "", url)
    # remove trailing slash if present
    url <- gsub("/$", "", url)

    try(
        writeLines(url, file.path(path, "CNAME")),
        silent = TRUE
    )
}
