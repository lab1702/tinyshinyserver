# Benchmark: Connection Count Cache vs Direct Iteration
# Compare O(1) cache lookup vs O(n) iteration for counting connections

library(devtools)
load_all(".")

benchmark_connection_counting <- function() {
  cat("=== Connection Count Performance Benchmark ===\n\n")

  # Test with different connection counts
  test_sizes <- c(10, 50, 100, 500, 1000)

  for (n_connections in test_sizes) {
    cat(sprintf("Testing with %d total connections across 5 apps...\n", n_connections))

    # Create fresh config
    config <- ShinyServerConfig$new()

    # Add connections distributed across 5 apps
    app_names <- paste0("app", 1:5)
    connections_per_app <- n_connections %/% 5

    for (i in 1:n_connections) {
      app_name <- app_names[((i - 1) %% 5) + 1]
      session_id <- sprintf("session-%d", i)

      config$add_ws_connection(session_id, list(
        ws = NULL,
        app_name = app_name,
        client_ip = "127.0.0.1",
        user_agent = "benchmark",
        last_activity = Sys.time(),
        created_at = Sys.time()
      ))
    }

    # Method 1: Cached O(1) lookup
    time_cached <- system.time({
      for (j in 1:1000) {
        for (app_name in app_names) {
          count <- config$get_app_connection_count(app_name)
        }
      }
    })

    # Method 2: Direct O(n) iteration
    count_connections_directly <- function(app_name) {
      count <- 0
      for (session_id in names(config$get_all_ws_connections())) {
        conn <- config$get_ws_connection(session_id)
        if (!is.null(conn) && !is.null(conn$app_name) && conn$app_name == app_name) {
          count <- count + 1
        }
      }
      return(count)
    }

    time_direct <- system.time({
      for (j in 1:1000) {
        for (app_name in app_names) {
          count <- count_connections_directly(app_name)
        }
      }
    })

    # Report results
    cat(sprintf("  Cached (O(1)):  %.4f seconds (%.2f µs per lookup)\n",
                time_cached["elapsed"],
                time_cached["elapsed"] * 1000000 / (1000 * 5)))

    cat(sprintf("  Direct (O(n)):  %.4f seconds (%.2f µs per lookup)\n",
                time_direct["elapsed"],
                time_direct["elapsed"] * 1000000 / (1000 * 5)))

    speedup <- time_direct["elapsed"] / time_cached["elapsed"]
    cat(sprintf("  Speedup:        %.1fx faster with cache\n\n", speedup))
  }

  cat("=== Analysis ===\n")
  cat("Cache complexity:\n")
  cat("  - Adds complexity to add/remove operations\n")
  cat("  - Requires validation function for consistency\n")
  cat("  - Has caused race condition bugs in the past\n\n")

  cat("Typical usage:\n")
  cat("  - Count queries: On connection close (once per connection lifetime)\n")
  cat("  - Status page: Every few seconds when viewing dashboard\n")
  cat("  - Most deployments: < 100 total connections\n\n")

  cat("Recommendation:\n")
  cat("  If speedup < 5x for realistic loads (< 100 connections),\n")
  cat("  consider removing cache for simpler, more maintainable code.\n")
}

# Run benchmark
benchmark_connection_counting()
