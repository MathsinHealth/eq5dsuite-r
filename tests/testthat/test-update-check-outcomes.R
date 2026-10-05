# update_value_sets() reports what it actually checked.
#
# A failed index download was skipped, so with every download failing nothing
# was "new", and that was read as success: "All value sets are up to date",
# all three instruments returned as checked, and today written as the date of
# the last check -- which silenced the overdue-update reminder for another 60
# days. With no internet connection it also returned every instrument as
# checked.
#
# Now each instrument is checked or failed, with a reason; "up to date" means
# every requested index was read and valid; and the last-checked date moves
# only when every requested index was verified and every install succeeded.
# Everything here is mocked: no test reaches the network.

VERSIONS <- c("3L", "5L", "Y3L")

# A valid index listing exactly what is installed: nothing new.
index_of_installed <- function(version, extra = character(0)) {
  codes <- c(get_installed_vs_codes(version), extra)
  data.frame(Version = version, Name = paste("Set", codes),
             Name_short = codes, Country_code = substr(codes, 1, 2),
             VS_code = codes, doi = "10.0/x", stringsAsFactors = FALSE)
}

# Run update_value_sets() with the network replaced. `fetch` maps a version
# to an index (or NULL, or an error).
run_update <- function(fetch, install = function(...) TRUE, internet = TRUE,
                       versions = VERSIONS) {
  local_mocked_bindings(
    .has_internet = function() internet,
    fetch_available_value_sets = fetch,
    install_value_set = install,
    apply_pending_migrations = function(...) invisible(list(status = "ok", applied = character(0), conflicts = character(0), failed = character(0), reason = NULL)),
    check_builtin_conflicts = function(...) invisible(character(0)))
  msgs <- character(0)
  res <- withCallingHandlers(
    update_value_sets(versions = versions, ask = FALSE),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  list(res = res, msgs = paste(msgs, collapse = ""))
}

last_checked_file <- function()
  file.path(get_cache_dir(create = FALSE), "last_checked.txt")

test_that("every download failing is not 'up to date', and moves no date", {
  local_eq_env()
  out <- run_update(function(version) NULL)
  expect_false(grepl("up to date", out$msgs))
  expect_identical(out$res$checked, character(0))
  expect_setequal(names(out$res$failed), VERSIONS)
  expect_false(file.exists(last_checked_file()))
  expect_identical(get_last_checked(), as.Date("1970-01-01"))
  expect_match(out$msgs, "not updated", fixed = TRUE)
})

test_that("an earlier last-checked date survives a failed check", {
  local_eq_env()
  set_last_checked(as.Date("2026-01-01"))
  run_update(function(version) NULL)
  expect_identical(get_last_checked(), as.Date("2026-01-01"))
  # So the reminder is still due.
  expect_true(is_update_due(threshold_days = 60))
})

test_that("a partial failure says which instruments were checked", {
  local_eq_env()
  out <- run_update(function(version)
    if (version == "5L") NULL else index_of_installed(version))
  expect_identical(out$res$checked, c("3L", "Y3L"))
  expect_identical(names(out$res$failed), "5L")
  expect_match(out$msgs, "EQ-5D-3L value sets are up to date", fixed = TRUE)
  expect_false(grepl("All value sets are up to date", out$msgs, fixed = TRUE))
  expect_false(file.exists(last_checked_file()))
})

test_that("a valid index with nothing new is up to date, and is recorded", {
  local_eq_env()
  out <- run_update(function(version) index_of_installed(version))
  expect_identical(out$res$checked, VERSIONS)
  expect_length(out$res$failed, 0L)
  expect_match(out$msgs, "All value sets are up to date", fixed = TRUE)
  expect_identical(get_last_checked(), Sys.Date())
})

test_that("an empty but valid index counts as checked", {
  local_eq_env()
  out <- run_update(function(version) index_of_installed(version)[0, ])
  expect_identical(out$res$checked, VERSIONS)
  expect_length(out$res$failed, 0L)
  expect_identical(get_last_checked(), Sys.Date())
})

test_that("a malformed index is a failure, not an empty list", {
  local_eq_env()
  bad <- list(
    no_code   = function(v) { x <- index_of_installed(v); x$VS_code <- NULL; x },
    not_frame = function(v) "<html>404</html>",
    blank     = function(v) { x <- index_of_installed(v); x$VS_code[1] <- ""; x },
    duplicate = function(v) { x <- index_of_installed(v); rbind(x, x[1, ]) })
  for (nm in names(bad)) {
    out <- run_update(bad[[nm]], versions = "3L")
    expect_identical(out$res$checked, character(0), info = nm)
    expect_match(out$res$failed[["3L"]], "malformed", info = nm)
    expect_false(file.exists(last_checked_file()), info = nm)
  }
})

test_that("a download that errors -- a timeout -- is a failure with its reason", {
  local_eq_env()
  out <- run_update(function(version) stop("Timeout was reached"),
                    versions = c("3L", "5L"))
  expect_identical(out$res$checked, character(0))
  expect_match(out$res$failed[["3L"]], "Timeout was reached", fixed = TRUE)
  expect_false(file.exists(last_checked_file()))
})

test_that("no internet connection checks nothing", {
  local_eq_env()
  out <- run_update(function(version) stop("must not be called"),
                    internet = FALSE)
  expect_identical(out$res$checked, character(0))
  expect_setequal(names(out$res$failed), VERSIONS)
  expect_false(file.exists(last_checked_file()))
})

test_that("a failed installation is reported and moves no date", {
  local_eq_env()
  out <- run_update(function(version)
    index_of_installed(version, extra = if (version == "5L") "ZZ_NEW"),
    install = function(vs_code, version, meta_row) FALSE)
  expect_identical(out$res$checked, VERSIONS)
  expect_identical(out$res$new, "ZZ_NEW")
  expect_identical(out$res$installed, character(0))
  expect_identical(out$res$install_failed, "ZZ_NEW")
  expect_false(file.exists(last_checked_file()))
  expect_match(out$msgs, "could not be installed", fixed = TRUE)
})

test_that("a successful installation is recorded", {
  local_eq_env()
  out <- run_update(function(version)
    index_of_installed(version, extra = if (version == "5L") "ZZ_NEW"))
  expect_identical(out$res$installed, "ZZ_NEW")
  expect_identical(out$res$install_failed, character(0))
  expect_identical(get_last_checked(), Sys.Date())
})
