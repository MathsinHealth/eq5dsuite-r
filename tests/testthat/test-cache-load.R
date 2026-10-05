# Issue 2: the cache must be version-checked, migrated where possible, and
# ignored safely otherwise.

test_that("a current cache round-trips and holds only the user's objects", {
  dir <- withr::local_tempdir()
  local_eq_env(dir)

  suppressMessages(
    eqvs_add(dummy_vs("3L", "FAN"), version = "3L", country = "Fantasia",
             countryCode = "FA", VSCode = "FAN", saveOption = 3,
             savePath = dir)
  )

  path <- file.path(dir, .cache_basename)
  expect_true(file.exists(path))

  saved <- new.env(parent = emptyenv())
  load(path, envir = saved)

  expect_identical(get("cache_schema_version", envir = saved),
                   .cache_schema_version)
  # Built-in data is never cached, so it cannot go stale.
  expect_false(any(c("country_codes", "crosswalk_NICE", "vsets3L_combined",
                     "xwsets", "xwrsets", "states_3L", "cache_path") %in%
                     ls(saved, all.names = TRUE)))
  expect_true("uservsets3L" %in% ls(saved))
  expect_true("user_defined_3L" %in% ls(saved))
})

test_that("cache_path is not restored from the cache file", {
  dir <- withr::local_tempdir()
  pkgenv <- local_eq_env(dir)

  # A cache carrying a stale path from another machine.
  write_cache_file(dir, cache_schema_version = .cache_schema_version,
                   cache_path = "/somewhere/that/does/not/exist")

  .apply_cache(pkgenv, dir)

  expect_identical(pkgenv$cache_path, dir)
})

test_that("a legacy cache with no schema version is migrated and kept", {
  dir <- withr::local_tempdir()
  pkgenv <- local_eq_env(dir)

  legacy <- legacy_user_objects("3L", "FAN")
  do.call(write_cache_file, c(list(dir = dir), legacy))   # no schema stamp

  expect_message(.apply_cache(pkgenv, dir), "migrated to version")

  # The user's value set survived.
  expect_true("FAN" %in% colnames(pkgenv$uservsets3L))
  expect_identical(nrow(pkgenv$user_defined_3L), 1L)
  expect_identical(pkgenv$user_defined_3L$VS_code, "FAN")
  expect_identical(pkgenv$user_defined_3L$doi, "doi:legacy")

  # ... and now matches the current schema, with citation added as NA.
  expect_length(.validate_vs_meta(pkgenv$user_defined_3L), 0L)
  expect_true(is.na(pkgenv$user_defined_3L$citation))

  # ... and is usable for scoring once the derived tables are rebuilt.
  .fixPkgEnv(saveCache = FALSE)
  expect_false(is.na(eq5d3l(11111, country = "FAN")))
})

test_that("a legacy cache is not rewritten at load time", {
  dir <- withr::local_tempdir()
  pkgenv <- local_eq_env(dir)

  legacy <- legacy_user_objects("3L", "FAN")
  path <- do.call(write_cache_file, c(list(dir = dir), legacy))
  before <- file.mtime(path)
  size_before <- file.size(path)

  suppressMessages(.apply_cache(pkgenv, dir))

  expect_identical(file.mtime(path), before)
  expect_identical(file.size(path), size_before)
})

test_that("the migrated cache is written on the next save", {
  dir <- withr::local_tempdir()
  local_eq_env(dir)

  legacy <- legacy_user_objects("3L", "FAN")
  do.call(write_cache_file, c(list(dir = dir), legacy))
  suppressMessages(.apply_cache(getOption("eq.env"), dir))
  .fixPkgEnv(saveCache = FALSE)

  suppressMessages(
    eqvs_add(dummy_vs("3L", "BBB", seed = 3), version = "3L", VSCode = "BBB",
             saveOption = 3, savePath = dir)
  )

  saved <- new.env(parent = emptyenv())
  load(file.path(dir, .cache_basename), envir = saved)

  expect_identical(get("cache_schema_version", envir = saved),
                   .cache_schema_version)
  ud <- get("user_defined_3L", envir = saved)
  expect_length(.validate_vs_meta(ud), 0L)
  expect_setequal(ud$VS_code, c("FAN", "BBB"))
})

test_that("a newer schema is ignored with a warning and built-ins still work", {
  dir <- withr::local_tempdir()
  pkgenv <- local_eq_env(dir)

  write_cache_file(dir,
                   cache_schema_version = .cache_schema_version + 1L,
                   uservsets3L = data.frame(state = 1L))

  expect_warning(.apply_cache(pkgenv, dir), "could not be used")

  # Nothing from the cache leaked into the package environment.
  expect_null(pkgenv$user_defined_3L)
  expect_identical(ncol(pkgenv$uservsets3L), 1L)

  # The built-in value sets are unaffected.
  .fixPkgEnv(saveCache = FALSE)
  expect_equal(unname(eq5d3l(11111, country = "GB")), 1)
  expect_equal(unname(eq5d5l(11111, country = "NL")), 1)
})

test_that("the warning for a newer schema names the file and the versions", {
  dir <- withr::local_tempdir()
  pkgenv <- local_eq_env(dir)

  path <- write_cache_file(dir,
                           cache_schema_version = 99L,
                           uservsets3L = data.frame(state = 1L))

  expect_warning(.apply_cache(pkgenv, dir), "cache schema version 99")
  expect_warning(.apply_cache(pkgenv, dir), basename(path), fixed = TRUE)
  expect_warning(.apply_cache(pkgenv, dir), "eqvs_add\\(\\)")
})

test_that("an unreadable cache is ignored with a warning", {
  dir <- withr::local_tempdir()
  pkgenv <- local_eq_env(dir)

  writeLines("this is not an RData file", file.path(dir, .cache_basename))

  expect_warning(.apply_cache(pkgenv, dir), "could not be read")
  expect_null(pkgenv$user_defined_3L)
})

test_that("a structurally broken cache is ignored with a warning", {
  dir <- withr::local_tempdir()
  pkgenv <- local_eq_env(dir)

  # Right schema stamp, wrong number of rows in the value matrix.
  write_cache_file(dir,
                   cache_schema_version = .cache_schema_version,
                   uservsets3L = data.frame(state = 1:10, FAN = runif(10)))

  expect_warning(.apply_cache(pkgenv, dir), "expected 243")
  expect_null(pkgenv$user_defined_3L)
})

test_that("values and metadata that disagree are treated as unusable", {
  dir <- withr::local_tempdir()
  pkgenv <- local_eq_env(dir)

  objs <- legacy_user_objects("3L", "FAN")
  # Metadata claims a value set the matrix does not hold.
  objs$user_defined_3L$VS_code <- "OTHER"

  do.call(write_cache_file,
          c(list(dir = dir), objs,
            list(cache_schema_version = .cache_schema_version)))

  expect_warning(.apply_cache(pkgenv, dir), "inconsistent")
  expect_null(pkgenv$user_defined_3L)
})

test_that("a rejected cache is left untouched until a save overwrites it", {
  dir <- withr::local_tempdir()
  local_eq_env(dir)

  path <- write_cache_file(dir,
                           cache_schema_version = 99L,
                           uservsets3L = data.frame(state = 1L))
  original <- readBin(path, "raw", file.size(path))

  suppressWarnings(.apply_cache(getOption("eq.env"), dir))

  # Load time leaves it exactly as it was, and creates no backup.
  expect_identical(readBin(path, "raw", file.size(path)), original)
  expect_length(list.files(dir, pattern = "\\.bak-"), 0L)

  # The backup appears only when a save is about to overwrite it.
  suppressMessages(
    eqvs_add(dummy_vs("3L", "FAN"), version = "3L", VSCode = "FAN",
             saveOption = 3, savePath = dir)
  )

  backups <- list.files(dir, pattern = "\\.bak-", full.names = TRUE)
  expect_length(backups, 1L)
  expect_match(basename(backups), "^cache\\.Rdta\\.bak-99-\\d{8}$")
  expect_identical(readBin(backups, "raw", file.size(backups)), original)

  # The new cache is readable and holds the new value set.
  saved <- new.env(parent = emptyenv())
  load(path, envir = saved)
  expect_identical(get("cache_schema_version", envir = saved),
                   .cache_schema_version)
  expect_true("FAN" %in% get("user_defined_3L", envir = saved)$VS_code)
})

# Whether this filesystem keeps "name." and "name" apart. Windows does not:
# the Win32 API drops a trailing dot, so "cache.Rdta." and "cache.Rdta" are
# one file there. Decided by trying it, not by the operating system's name.
trailing_dot_distinct <- function(dir) {
  probe <- file.path(dir, "probe.")
  writeLines("x", probe)
  on.exit(unlink(c(probe, file.path(dir, "probe"))), add = TRUE)
  !file.exists(file.path(dir, "probe"))
}

test_that("the legacy 'cache.Rdta.' filename is read as a fallback", {
  dir <- withr::local_tempdir()
  pkgenv <- local_eq_env(dir)

  legacy <- legacy_user_objects("3L", "FAN")
  path <- do.call(write_cache_file,
                  c(list(dir = dir), legacy,
                    list(basename = .cache_basename_legacy)))
  expect_true(file.exists(path))

  if (trailing_dot_distinct(dir)) {
    # Linux, macOS: two names, and the legacy one is found as such.
    expect_identical(.find_cache_file(dir), path)
  } else {
    # Windows: writing "cache.Rdta." made "cache.Rdta". The one file is
    # found under its current name, which is the same file.
    expect_identical(list.files(dir), .cache_basename)
    expect_identical(.find_cache_file(dir), file.path(dir, .cache_basename))
  }
  suppressMessages(.apply_cache(pkgenv, dir))
  expect_true("FAN" %in% colnames(pkgenv$uservsets3L))
})

test_that("where the two names are one file, legacy value sets survive a save", {
  # Simulates Windows on any platform: the legacy cache is the file under the
  # current name, and the legacy name leads to it.
  dir <- withr::local_tempdir()
  pkgenv <- local_eq_env(dir)
  legacy <- legacy_user_objects("3L", "FAN")
  current <- do.call(write_cache_file,
                     c(list(dir = dir), legacy,
                       list(basename = .cache_basename)))
  linked <- suppressWarnings(file.symlink(current,
                                          file.path(dir, .cache_basename_legacy)))
  skip_if_not(isTRUE(linked), "symbolic links are not available here")

  expect_identical(.find_cache_file(dir), current)
  suppressMessages(.apply_cache(pkgenv, dir))
  .fixPkgEnv(saveCache = FALSE)
  suppressMessages(
    eqvs_add(dummy_vs("3L", "BBB", seed = 4), version = "3L", VSCode = "BBB",
             saveOption = 3, savePath = dir))

  # Saved under the current name, keeping the legacy set beside the new one.
  saved <- new.env(parent = emptyenv())
  load(current, envir = saved)
  expect_true(all(c("FAN", "BBB") %in% get("user_defined_3L", envir = saved)$VS_code))
  expect_true(file.exists(file.path(dir, .cache_basename_legacy)))
})

test_that("the legacy file is never renamed or removed", {
  dir <- withr::local_tempdir()
  local_eq_env(dir)

  legacy <- legacy_user_objects("3L", "FAN")
  legacy_path <- do.call(write_cache_file,
                         c(list(dir = dir), legacy,
                           list(basename = .cache_basename_legacy)))
  suppressMessages(.apply_cache(getOption("eq.env"), dir))
  .fixPkgEnv(saveCache = FALSE)

  suppressMessages(
    eqvs_add(dummy_vs("3L", "BBB", seed = 4), version = "3L", VSCode = "BBB",
             saveOption = 3, savePath = dir)
  )

  expect_true(file.exists(legacy_path))
  expect_true(file.exists(file.path(dir, .cache_basename)))
})

test_that("the current basename wins over the legacy one", {
  dir <- withr::local_tempdir()
  local_eq_env(dir)

  write_cache_file(dir, cache_schema_version = .cache_schema_version)
  write_cache_file(dir, cache_schema_version = 1L,
                   basename = .cache_basename_legacy)

  expect_identical(.find_cache_file(dir), file.path(dir, .cache_basename))
})

test_that(".find_cache_file() copes with a missing directory", {
  expect_null(.find_cache_file(file.path(tempdir(), "no-such-dir-xyz")))
  expect_null(.find_cache_file(NULL))
  expect_null(.find_cache_file(""))
})

test_that("an unwritable cache location warns rather than errors", {
  dir <- withr::local_tempdir()
  pkgenv <- local_eq_env(dir)

  # A regular file standing where the cache directory should be.
  blocked <- file.path(dir, "blocked")
  writeLines("not a directory", blocked)

  expect_warning(
    result <- .save_cache(pkgenv, file.path(blocked, .cache_basename)),
    "could not write the value set cache"
  )
  expect_false(result)
})

test_that("saving an empty cache is still valid", {
  dir <- withr::local_tempdir()
  pkgenv <- local_eq_env(dir)

  expect_true(.save_cache(pkgenv, file.path(dir, .cache_basename)))

  saved <- new.env(parent = emptyenv())
  load(file.path(dir, .cache_basename), envir = saved)
  expect_identical(get("cache_schema_version", envir = saved),
                   .cache_schema_version)
})

test_that("eqvs_load() accepts a legacy cache and makes its sets usable", {
  dir <- withr::local_tempdir()
  local_eq_env()

  legacy <- legacy_user_objects("3L", "FAN")
  do.call(write_cache_file, c(list(dir = dir), legacy))

  expect_true(suppressMessages(eqvs_load(dir)))
  expect_false(is.na(eq5d3l(11111, country = "FAN")))
})

test_that("eqvs_load() returns FALSE for a cache it cannot use", {
  dir <- withr::local_tempdir()
  local_eq_env()

  write_cache_file(dir, cache_schema_version = 99L,
                   uservsets3L = data.frame(state = 1L))

  expect_warning(result <- eqvs_load(dir), "could not be used")
  expect_false(result)
})

test_that("eqvs_load() still errors when there is no cache at all", {
  dir <- withr::local_tempdir()
  local_eq_env()

  expect_error(eqvs_load(dir), "Cache file not found")
  expect_error(eqvs_load(""), "valid 'loadPath'")
})
