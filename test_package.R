library(tinyshinyserver)

cat("Loading tinyshinyserver package...\n")

# Test 1: Check exported functions
cat("Exported functions:", ls("package:tinyshinyserver"), "\n")

# Test 2: Check system files
template_path <- system.file("templates", package = "tinyshinyserver")
cat("Template path:", template_path, "\n")
cat("Template files:", list.files(template_path, recursive = TRUE), "\n")

examples_path <- system.file("examples", package = "tinyshinyserver")
cat("Examples path:", examples_path, "\n")
cat("Example files:", list.files(examples_path), "\n")

# Test 3: Test server initialization (without starting)
tryCatch({
  server <- TinyShinyServer$new("./simple-test.json")
  cat("Server initialized successfully\n")
  cat("Config loaded with", length(server$config$config$apps), "apps\n")
  cat("App names:", sapply(server$config$config$apps, function(x) x$name), "\n")
}, error = function(e) {
  cat("Error initializing server:", e$message, "\n")
})

cat("Package test completed.\n")