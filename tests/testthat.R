# The tests must never read or write the user's value set cache, and that
# includes .onLoad() and .onAttach(), which run before any test code. They
# find the cache through tools::R_user_dir(), which honours R_USER_CACHE_DIR,
# so point it at an empty temporary directory before the package is loaded.
Sys.setenv(R_USER_CACHE_DIR = tempfile("eq5dsuite-test-cache-"))

library(testthat)
library(eq5dsuite)

test_check("eq5dsuite")
