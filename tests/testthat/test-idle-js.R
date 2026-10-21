# The idle watch (inst/shiny/WWW/idle.js) runs in the browser in online mode
# only, so no server test executes it. Shiny (>= 1.8) refuses a custom message
# handler that does not take exactly one argument, and throws; the throw came
# from inside the script's "shiny:connected" hook and stopped the online app
# from loading the example data or an upload, while the local app, which does
# not include the script, worked. Handlers must also be registered once:
# registering one again on a reconnect throws as well.

idle_js <- function() {
  f <- file.path(app_dir(), "WWW", "idle.js")
  skip_if(!file.exists(f), "bundled Shiny app not available")
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

test_that("every custom message handler takes exactly one argument", {
  js <- idle_js()
  calls <- regmatches(js, gregexpr(
    "addCustomMessageHandler\\(\\s*\"[^\"]+\"\\s*,\\s*function\\s*\\(([^)]*)\\)", js))[[1L]]
  expect_gt(length(calls), 0L)
  args <- sub(".*function\\s*\\(([^)]*)\\)$", "\\1", calls)
  n_args <- lengths(strsplit(trimws(args), "\\s*,\\s*"))
  n_args[!nzchar(trimws(args))] <- 0L
  expect_true(all(n_args == 1L), info = paste(calls, collapse = "\n"))
})

test_that("the handlers are registered once, not on every connection", {
  js <- idle_js()
  # Nothing is registered inside a shiny:connected (or reconnected) hook.
  hooks <- regmatches(js, gregexpr("on\\(\"shiny:(re)?connected\"[\\s\\S]*?\\}\\);", js, perl = TRUE))[[1L]]
  expect_false(any(grepl("addCustomMessageHandler", hooks)))
  expect_identical(lengths(regmatches(js, gregexpr("addCustomMessageHandler", js))), 2L)
})
