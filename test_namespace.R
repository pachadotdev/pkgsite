devtools::load_all()

# Test the function directly with namespace
test_desc <- "\\code{build_site()} creates a simple static documentation site"

cat("Testing with pkgsite::: namespace:\n")
result <- pkgsite:::clean_rd_content(test_desc)
cat("Result:", result, "\n")

# Also test if the function exists at all
cat("Function exists:", exists("clean_rd_content", envir = asNamespace("pkgsite")), "\n")
