library(tinyshinyserver)

cat("Testing start_tss function...\n")

# Test 1: Check that start_tss exists
cat("start_tss function exists:", exists("start_tss"), "\n")

# Test 2: Check parameter validation
tryCatch({
  start_tss("non-existent-file.json")
}, error = function(e) {
  cat("Expected error for non-existent file:", e$message, "\n")
})

# Test 3: Show that we have the function ready
cat("start_tss function is ready for use!\n")
cat("Usage: start_tss(config = 'path/to/config.json')\n")