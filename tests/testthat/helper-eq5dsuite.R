# Helpers shared by the eq5dsuite tests.
#
# Every helper here works on an isolated package environment backed by a
# temporary directory, so the tests never read or write the user's real cache.

# Install a fresh package environment whose cache_path points at a temporary
# directory, and restore the previous one when the calling test finishes.
local_eq_env <- function(cache_dir = NULL, .local_envir = parent.frame()) {
  if (is.null(cache_dir))
    cache_dir <- withr::local_tempdir(.local_envir = .local_envir)
  pkgenv <- new.env(parent = emptyenv())
  assign("cache_path", cache_dir, envir = pkgenv)
  withr::local_options(list(eq.env = pkgenv), .local_envir = .local_envir)
  .fixPkgEnv(saveCache = FALSE)
  pkgenv
}

# A syntactically valid but meaningless value set.
dummy_vs <- function(version = "3L", code = "FAN", seed = 1) {
  n <- if (version == "5L") 3125L else 243L
  withr::with_seed(seed, {
    df <- data.frame(state = make_all_EQ_indexes(version),
                     value = round(stats::runif(n, -0.5, 1), 4))
  })
  names(df)[2] <- code
  df
}

# Write an arbitrary set of objects to a cache file.
write_cache_file <- function(dir, ..., basename = .cache_basename) {
  objs <- list(...)
  e <- new.env(parent = emptyenv())
  for (nm in names(objs)) assign(nm, objs[[nm]], envir = e)
  path <- file.path(dir, basename)
  save(list = ls(e, all.names = TRUE), envir = e, file = path)
  path
}

# Build the pieces of a legacy (schema 1) cache: no cache_schema_version, and a
# user_defined_* table without the `citation` column, as written by
# eq5dsuite <= 2.0.0.
legacy_user_objects <- function(version = "3L", code = "FAN") {
  vs <- dummy_vs(version, code)
  states <- make_all_EQ_states(version = if (version == "5L") "5L" else "3L",
                               append_index = TRUE)
  uservsets <- states[, "state", drop = FALSE]
  uservsets[[code]] <- vs[[2]]

  user_defined <- data.frame(
    Version      = version,
    Name         = "Fantasia",
    Name_short   = "Fantasia",
    Country_code = "FA",
    VS_code      = code,
    doi          = "doi:legacy",
    stringsAsFactors = FALSE
  )

  out <- list(uservsets, user_defined)
  names(out) <- c(paste0("uservsets", version),
                  paste0("user_defined_", version))
  out
}

# Run an expression that prints, returning its messages and discarding the
# printed table, so test output stays clean.
printed_messages <- function(expr) {
  capture.output(
    invisible(capture.output(expr)),
    type = "message"
  )
}
