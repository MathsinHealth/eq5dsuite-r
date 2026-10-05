# Isolate the whole suite from the user's value set cache.
#
# testthat runs setup files after the package is loaded and before any test
# file. From here on the package environment is a fresh one backed by an
# empty temporary cache, holding only the built-in value sets, so no test can
# see a value set the user added -- a stale user-defined "UK" set once made
# tests pass that fail on a clean machine. R_USER_CACHE_DIR is redirected too,
# so anything that resolves the cache directory afresh lands in the same
# place. Both are restored when the run ends.
#
# Loading itself happens before this file runs. tests/testthat.R sets
# R_USER_CACHE_DIR before library(), which isolates loading under R CMD
# check. devtools::test() loads the package before any test code, so there
# only the caller can isolate it -- see test-cache-isolation.R.

local({
  # The cache directory the package was initialised against, recorded before
  # it is replaced, for test-cache-isolation.R.
  options(eq5dsuite.test.load_cache_path = getOption("eq.env")$cache_path)
  withr::defer(options(eq5dsuite.test.load_cache_path = NULL),
               envir = testthat::teardown_env())

  dir <- withr::local_tempdir("eq5dsuite-test-cache-",
                              .local_envir = testthat::teardown_env())
  withr::local_envvar(R_USER_CACHE_DIR = dir,
                      .local_envir = testthat::teardown_env())

  pkgenv <- new.env(parent = emptyenv())
  assign("cache_path", find_cache_dir("eq5dsuite"), envir = pkgenv)
  withr::local_options(list(eq.env = pkgenv),
                       .local_envir = testthat::teardown_env())
  .fixPkgEnv(saveCache = FALSE)
})

# No test may reach CRAN: the app's version check is off for the whole run.
# test-version-check.R mocks the request and turns it back on locally.
withr::local_options(list(eq5dsuite.check_cran = FALSE),
                     .local_envir = testthat::teardown_env())

