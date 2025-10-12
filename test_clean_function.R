# Reload to get latest changes
devtools::load_all()

# Test the function directly
test_desc <- "\n\\code{build_site()} creates a simple static documentation site with:\n\\itemize{\n\\item \\code{\\link[=init_site]{init_site()}}\n\\item \\code{\\link[=build_home]{build_home()}}\n\\item \\code{\\link[=build_reference]{build_reference()}}\n\\item \\code{\\link[=build_vignettes]{build_vignettes()}}\n}\n\nThe site uses a minimal template with a simple sidebar navigation.\n\n"

cat("Input:\n")
cat(test_desc)
cat("\n\nOutput:\n")

result <- clean_rd_content(test_desc)
cat(result)
