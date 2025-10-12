# Test individual patterns
test_text <- "\\code{build_site()} creates a simple static documentation site with:"

cat("Original:", test_text, "\n")

# Test \code{} pattern
result1 <- gsub("\\\\code\\{([^}]+)\\}", "<code>\\1</code>", test_text, perl = TRUE)
cat("After code processing:", result1, "\n")

# Test a simple text with link
test_text2 <- "\\code{\\link[=init_site]{init_site()}}"
cat("Original2:", test_text2, "\n")

result2 <- gsub("\\\\code\\{\\\\link\\[=([^\\]]+)\\]\\{([^}]+)\\}\\}", '<code><a href="../reference/\\1.html">\\2</a></code>', test_text2, perl = TRUE)
cat("After nested processing:", result2, "\n")
