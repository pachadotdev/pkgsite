library(pkgsite)

# Read the .Rd file
rd_content <- readLines("man/build_site.Rd")
rd_content <- paste(rd_content, collapse = "\n")

# Extract just the description section manually
desc_start <- regexpr("\\\\description\\{", rd_content)[1]
if (desc_start > 0) {
  char_vec <- strsplit(rd_content, "")[[1]]
  brace_count <- 0
  start_idx <- desc_start + nchar("\\description{") - 1
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
    desc_section <- substr(rd_content, desc_start, end_idx)
    # Use the new extraction method
    desc_content <- substr(desc_section, nchar("\\description{") + 1, nchar(desc_section) - 1)
    
    cat("Raw description content:\n")
    cat(desc_content)
    cat("\n\n")
    
    # Process it with our function
    processed <- pkgsite:::clean_rd_content(desc_content)
    cat("Processed content:\n")
    cat(processed)
    cat("\n")
  }
}
