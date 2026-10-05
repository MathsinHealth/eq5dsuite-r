# The suite must neither read nor write the user's value set cache. See
# setup-cache-isolation.R for how it is isolated, and tests/testthat.R for
# isolating package loading.

# The cache directory the user would have, ignoring any redirection.
real_cache_dir <- function() {
  withr::with_envvar(c(R_USER_CACHE_DIR = NA), find_cache_dir("eq5dsuite"))
}

same_path <- function(a, b) {
  identical(normalizePath(a, mustWork = FALSE), normalizePath(b, mustWork = FALSE))
}

test_that("the tests run against a temporary cache, not the user's", {
  pkgenv <- getOption("eq.env")
  expect_false(same_path(pkgenv$cache_path, real_cache_dir()))
  expect_false(same_path(get_cache_dir(create = FALSE), real_cache_dir()))
  expect_false(same_path(find_cache_dir("eq5dsuite"), real_cache_dir()))
})

test_that("the tests see only the built-in value sets", {
  pkgenv <- getOption("eq.env")
  for (v in c("3L", "5L", "Y3L")) {
    ud <- pkgenv[[paste0("user_defined_", v)]]
    expect_true(is.null(ud) || nrow(ud) == 0L, info = v)
  }
  # The stale user-defined "UK" set that once made app tests pass.
  expect_false("UK" %in% eqvs_display(version = "3L", return_df = TRUE)$VS_code)
})

test_that("the package was loaded without the user's cache", {
  load_path <- getOption("eq5dsuite.test.load_cache_path")
  skip_if(is.null(load_path), "load-time cache path not recorded")
  # devtools::test() loads the package before any test code runs, so only the
  # caller can redirect the cache for .onLoad() and .onAttach(). R CMD check
  # is isolated by tests/testthat.R.
  skip_if(same_path(load_path, real_cache_dir()),
          paste0("the package was loaded against the user's cache; run ",
                 "with R_USER_CACHE_DIR set to an empty temporary directory ",
                 "to isolate loading too"))
  expect_false(same_path(load_path, real_cache_dir()))
})

test_that("reading the cache's bookkeeping creates no directory", {
  dir <- file.path(withr::local_tempdir(), "not-yet")
  local_eq_env(cache_dir = dir)
  expect_identical(get_last_checked(), as.Date("1970-01-01"))
  expect_identical(get_applied_migrations(), character(0))
  expect_false(is_update_due(threshold_days = 1e6))
  expect_false(dir.exists(dir))

  # Writing still creates it.
  set_last_checked(as.Date("2026-01-01"))
  expect_true(dir.exists(dir))
  expect_identical(get_last_checked(), as.Date("2026-01-01"))
})
