# An example must leave the session as it found it. CRAN requires that of
# options and the working directory, and a user who runs example() should not
# find their random seed moved or files left in their project either.
#
# Every exported function's example is checked at once, rather than one at a
# time: tools::Rd2ex() turns each Rd into the runnable script R CMD check runs,
# with \dontrun commented out, and each is run between two snapshots of the
# session. The whole set takes a few seconds.

# The Rd sources, not the installed help: under R CMD check the sources are
# absent, so this skips, exactly as the "no package code changes the working
# directory" test does.
rd_source_dir <- function() testthat::test_path("..", "..", "man")

# Everything an example is not allowed to disturb.
session_snapshot <- function(dir) {
  list(
    options = options(),
    wd      = getwd(),
    env     = as.list(Sys.getenv()),
    seed    = if (exists(".Random.seed", envir = globalenv()))
                get(".Random.seed", envir = globalenv()),
    files   = sort(list.files(dir, recursive = TRUE, all.files = TRUE))
  )
}

# Names added, removed, or left holding a different value.
entry_changes <- function(before, after) {
  added   <- setdiff(names(after), names(before))
  removed <- setdiff(names(before), names(after))
  common  <- intersect(names(before), names(after))
  differs <- common[!vapply(common, function(n)
    identical(before[[n]], after[[n]]), logical(1L))]
  c(if (length(added))   paste0("set ", added),
    if (length(removed)) paste0("unset ", removed),
    if (length(differs)) paste0("changed ", differs))
}

# What one example disturbed, as one line per kind. Empty means it is clean.
example_trace <- function(script, dir) {
  before <- session_snapshot(dir)
  failure <- NULL
  tryCatch(
    suppressMessages(suppressWarnings(
      sys.source(script, envir = new.env(parent = globalenv())))),
    error = function(e) failure <<- conditionMessage(e))
  after <- session_snapshot(dir)

  opts <- entry_changes(before$options, after$options)
  envs <- entry_changes(before$env, after$env)
  c(if (length(opts)) paste("options:", paste(opts, collapse = ", ")),
    if (!identical(before$wd, after$wd))
      paste("working directory:", before$wd, "->", after$wd),
    if (length(envs)) paste("environment variables:", paste(envs, collapse = ", ")),
    if (!identical(before$seed, after$seed)) "random seed moved",
    if (length(setdiff(after$files, before$files)))
      paste("files written outside tempdir():",
            paste(setdiff(after$files, before$files), collapse = ", ")),
    if (!is.null(failure)) paste("failed:", failure))
}

test_that("no example changes options, the working directory, the seed, or writes files", {
  skip_if_not_installed("withr")
  man <- rd_source_dir()
  skip_if(!dir.exists(man), "package sources not available")
  rds <- sort(list.files(man, pattern = "\\.Rd$", full.names = TRUE))
  skip_if(!length(rds), "package sources not available")
  # Absolute, because the working directory moves below.
  rds <- normalizePath(rds)

  # A directory of its own, so a file an example writes relative to "." shows
  # up here rather than in the test directory. tempdir() itself is fair game:
  # examples are allowed to write there, and several do.
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  # The plot examples would otherwise leave an Rplots.pdf behind.
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())

  trace <- character(0L)
  for (rd in rds) {
    script <- tempfile(fileext = ".R")
    tools::Rd2ex(rd, out = script)
    if (!file.exists(script)) next          # an Rd with no examples
    found <- example_trace(script, dir)
    if (length(found))
      trace <- c(trace, paste0(basename(rd), ": ", found))
  }

  expect_identical(trace, character(0L))
})
