# A user-defined value set whose code a built-in set already uses cannot work:
# .fixPkgEnv() joins the built-in and user-defined tables, and merge() joins on
# every shared column name, so the shared code becomes part of the key and the
# combined table collapses to nothing.
#
# eqvs_add() refuses such a code (test-eqvs-add-collision.R), but that only
# guards new additions. The realistic case reaches the package from the cache:
# a value set someone added by hand under, say, "NG" before Nigeria shipped
# with the package. Upgrading then broke every EQ-5D-5L lookup, with
# eqvs_drop() the only way out.
#
# Such a set is now dropped as the cache is read, with a warning naming it. The
# user's other value sets are kept, and the file on disk is not touched.

# Build the pair of objects an older eq5dsuite would have cached.
user_objects <- function(pkgenv, codes, version = "5L") {
  n <- if (version == "5L") 3125L else 243L
  u <- pkgenv[[paste0("uservsets", version)]]
  meta <- NULL
  for (cc in codes) {
    u[[cc]] <- round(seq(1, -0.5, length.out = n), 4)
    meta <- rbind(meta, .new_vs_meta_row(
      Version = version, Name = cc, Name_short = cc,
      Country_code = cc, VS_code = cc, doi = NA_character_))
  }
  out <- list(u, meta)
  names(out) <- c(paste0("uservsets", version), paste0("user_defined_", version))
  out
}

# Write such a cache, load it into a fresh environment, and return the warnings.
load_cache <- function(dir, objects) {
  do.call(write_cache_file,
          c(list(dir), objects, list(cache_schema_version = .cache_schema_version)))
  pkgenv <- local_eq_env(dir, .local_envir = parent.frame())
  w <- character(0)
  withCallingHandlers(
    .apply_cache(pkgenv, dir),
    warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
  .fixPkgEnv(saveCache = FALSE)
  list(pkgenv = pkgenv, warnings = w)
}

# ---------------------------------------------------------------------------
# The helpers
# ---------------------------------------------------------------------------

test_that(".colliding_vs_codes() picks out built-in codes, whatever the case", {
  expect_identical(.colliding_vs_codes(c("FAN", "GB", "ng", "XX")),
                   c("GB", "ng"))
  expect_identical(.colliding_vs_codes(c("FAN", "XX")), character(0))
  expect_identical(.colliding_vs_codes(character(0)), character(0))
  # Suffixed codes are matched in full, not by prefix.
  expect_identical(.colliding_vs_codes(c("DE_TTO", "DE_MINE")), "DE_TTO")
})

test_that(".combine_vsets() joins on state and drops a colliding column", {
  builtin <- data.frame(state = 1:3, GB = c(1, 2, 3), US = c(4, 5, 6))
  user    <- data.frame(state = 1:3, FAN = c(7, 8, 9))

  ok <- .combine_vsets(builtin, user)
  expect_identical(names(ok), c("state", "GB", "US", "FAN"))
  expect_identical(nrow(ok), 3L)

  # A colliding column would otherwise become part of the join key.
  bad <- .combine_vsets(builtin, data.frame(state = 1:3, GB = c(9, 9, 9),
                                            FAN = c(7, 8, 9)))
  expect_identical(names(bad), c("state", "GB", "US", "FAN"))
  expect_identical(nrow(bad), 3L)
  expect_identical(bad$GB, c(1, 2, 3))      # the built-in values, not 9s
  expect_identical(bad$FAN, c(7, 8, 9))     # the other user set survives

  # Case variants too.
  lower <- .combine_vsets(builtin, data.frame(state = 1:3, gb = c(9, 9, 9)))
  expect_identical(names(lower), c("state", "GB", "US"))

  # A user table holding nothing but `state` is the ordinary case.
  expect_identical(nrow(.combine_vsets(builtin, builtin[, "state", drop = FALSE])),
                   3L)
})

# ---------------------------------------------------------------------------
# Loading a cache that carries a collision
# ---------------------------------------------------------------------------

test_that("a colliding value set is dropped from the cache, with a warning", {
  dir <- withr::local_tempdir()
  pkgenv <- local_eq_env(dir)
  r <- load_cache(dir, user_objects(pkgenv, c("FAN", "GB"), "5L"))

  expect_length(r$warnings, 1L)
  expect_match(r$warnings, "whose code a built-in value set already uses",
               fixed = TRUE)
  expect_match(r$warnings, "EQ-5D-5L: GB", fixed = TRUE)
  expect_match(r$warnings, "eqvs_add()", fixed = TRUE)

  # The combined table is whole, and the built-in set is reachable again.
  expect_identical(nrow(r$pkgenv$vsets5L_combined), 3125L)
  expect_equal(unname(eq5d5l(11111, country = "GB")), 1)
})

test_that("the user's other value sets are kept", {
  dir <- withr::local_tempdir()
  pkgenv <- local_eq_env(dir)
  r <- load_cache(dir, user_objects(pkgenv, c("FAN", "GB"), "5L"))

  df <- eqvs_display(version = "5L", return_df = TRUE)
  expect_true("FAN" %in% df$VS_code)
  expect_equal(unname(eq5d5l(11111, country = "FAN")), 1)

  # The dropped one is gone from both tables, so they stay in step.
  expect_false("GB" %in% colnames(r$pkgenv$uservsets5L))
  expect_false("GB" %in% r$pkgenv$user_defined_5L$VS_code)
  expect_identical(sum(df$VS_code == "GB"), 1L)   # the built-in, once
})

test_that("a case variant of a built-in code is dropped too", {
  dir <- withr::local_tempdir()
  pkgenv <- local_eq_env(dir)
  r <- load_cache(dir, user_objects(pkgenv, "gb", "5L"))

  expect_match(r$warnings, "EQ-5D-5L: gb", fixed = TRUE)
  # Without the fix this left GB ambiguous rather than collapsing the table.
  expect_equal(unname(eq5d5l(11111, country = "GB")), 1)
})

test_that("a code built in for another instrument is dropped", {
  # NG is an EQ-5D-5L code; here it is cached as a user-defined 3L set.
  dir <- withr::local_tempdir()
  pkgenv <- local_eq_env(dir)
  r <- load_cache(dir, user_objects(pkgenv, "NG", "3L"))

  expect_match(r$warnings, "EQ-5D-3L: NG", fixed = TRUE)
  expect_identical(nrow(r$pkgenv$vsets3L_combined), 243L)
  expect_false("NG" %in% eqvs_display(version = "3L", return_df = TRUE)$VS_code)
})

test_that("collisions across two instruments are reported together", {
  dir <- withr::local_tempdir()
  pkgenv <- local_eq_env(dir)
  objects <- c(user_objects(pkgenv, c("FAN", "GB"), "5L"),
               user_objects(pkgenv, "ng", "3L"))
  r <- load_cache(dir, objects)

  expect_length(r$warnings, 1L)
  expect_match(r$warnings, "EQ-5D-3L: ng", fixed = TRUE)
  expect_match(r$warnings, "EQ-5D-5L: GB", fixed = TRUE)
  expect_true("FAN" %in% eqvs_display(version = "5L", return_df = TRUE)$VS_code)
})

test_that("the cache file itself is not modified", {
  dir <- withr::local_tempdir()
  pkgenv <- local_eq_env(dir)
  do.call(write_cache_file,
          c(list(dir), user_objects(pkgenv, c("FAN", "GB"), "5L"),
            list(cache_schema_version = .cache_schema_version)))
  path <- file.path(dir, .cache_basename)
  before <- tools::md5sum(path)

  local_eq_env(dir)
  suppressWarnings(.apply_cache(getOption("eq.env"), dir))

  expect_identical(tools::md5sum(path), before)

  # ... and the file still holds the dropped set, until a save rewrites it.
  saved <- new.env(parent = emptyenv())
  load(path, envir = saved)
  expect_true("GB" %in% colnames(get("uservsets5L", envir = saved)))
})

test_that("saving afterwards writes the cleaned value sets", {
  dir <- withr::local_tempdir()
  pkgenv <- local_eq_env(dir)
  r <- load_cache(dir, user_objects(pkgenv, c("FAN", "GB"), "5L"))

  expect_true(.save_cache(r$pkgenv, file.path(dir, .cache_basename)))

  saved <- new.env(parent = emptyenv())
  load(file.path(dir, .cache_basename), envir = saved)
  cols <- colnames(get("uservsets5L", envir = saved))
  expect_false("GB" %in% cols)
  expect_true("FAN" %in% cols)
})

# ---------------------------------------------------------------------------
# No collision, no change
# ---------------------------------------------------------------------------

test_that("an ordinary cache loads without a collision warning", {
  dir <- withr::local_tempdir()
  pkgenv <- local_eq_env(dir)
  r <- load_cache(dir, user_objects(pkgenv, c("FAN", "XX"), "5L"))

  expect_length(r$warnings, 0L)
  expect_identical(nrow(r$pkgenv$vsets5L_combined), 3125L)
  for (code in c("FAN", "XX"))
    expect_true(code %in% eqvs_display(version = "5L",
                                       return_df = TRUE)$VS_code)
})

test_that("a legacy cache is still migrated, and a collision handled with it", {
  dir <- withr::local_tempdir()
  pkgenv <- local_eq_env(dir)

  # Schema 1: no cache_schema_version stamp, and metadata with no citation.
  objects <- user_objects(pkgenv, c("FAN", "GB"), "5L")
  objects$user_defined_5L$citation <- NULL

  do.call(write_cache_file, c(list(dir), objects))
  pkgenv <- local_eq_env(dir)
  w <- character(0)
  msg <- suppressMessages(withCallingHandlers(
    .apply_cache(pkgenv, dir),
    warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") }))

  expect_identical(msg, "migrated")
  expect_length(w, 1L)
  expect_match(w, "EQ-5D-5L: GB", fixed = TRUE)
  expect_false("GB" %in% colnames(pkgenv$uservsets5L))
  expect_true("FAN" %in% colnames(pkgenv$uservsets5L))
  expect_true("citation" %in% colnames(pkgenv$user_defined_5L))
})
