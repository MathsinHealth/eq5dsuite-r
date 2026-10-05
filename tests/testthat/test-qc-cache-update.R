# Review of 2026-10-04, group 3: cache and update postconditions.
#
# Q04 a migration renaming QCA to QCB, when a different QCB already existed,
#     was marked applied although nothing was renamed.
# Q05 when the unreadable cache had to be backed up before being replaced and
#     the backup failed, the save went ahead: the bytes were replaced and no
#     backup existed.
# Q11 a migrations list that could not be downloaded was treated like an
#     empty one: "All value sets are up to date", and the last-checked date
#     moved to today.
#
# Every test runs on a temporary cache with mocked downloads.

q <- function(expr) suppressWarnings(suppressMessages(expr))

# A 3L value set whose full-health value is `top`.
vs3 <- function(code, top) {
  s <- make_all_EQ_indexes("3L")
  df <- data.frame(state = s, v = seq(top, -0.5, length.out = length(s)))
  names(df)[2] <- code
  df
}

add3 <- function(code, top)
  q(eqvs_add(vs3(code, top), version = "3L", country = paste("Test", code),
             countryCode = substr(code, 1, 2), VSCode = code, saveOption = 1))

migration <- function(old, new, version = "3L")
  data.frame(version = version, old_VS_code = old, new_VS_code = new,
             reason = "test", date = "2026-10-04", stringsAsFactors = FALSE)

installed <- function(v = "3L") get_installed_vs_codes(v)
score <- function(code) q(eq5d(11111, country = code, version = "3L"))

run_migrations <- function(m, env = parent.frame()) {
  local_mocked_bindings(fetch_migrations = function() m, .env = env)
  q(apply_pending_migrations(ask = FALSE))
}

# ── Q04 ───────────────────────────────────────────────────────────────────────

test_that("a rename onto a different existing set is a conflict, not applied", {
  local_eq_env()
  add3("QCA", 1); add3("QCB", 0.5)
  res <- run_migrations(migration("QCA", "QCB"))
  expect_identical(res$status, "conflict")
  expect_identical(res$conflicts, "3L:QCA->QCB")
  expect_length(res$applied, 0L)
  expect_false("3L:QCA->QCB" %in% get_applied_migrations())
  # Both sets are still there, with their own values.
  expect_true(all(c("QCA", "QCB") %in% installed()))
  expect_equal(score("QCA"), 1)
  expect_equal(score("QCB"), 0.5)
  # Still pending: the next run reports it again.
  expect_identical(run_migrations(migration("QCA", "QCB"))$conflicts,
                   "3L:QCA->QCB")
})

test_that("an equal target is a conflict too: nothing is deleted without a policy", {
  local_eq_env()
  add3("QCA", 1); add3("QCB", 1)
  res <- run_migrations(migration("QCA", "QCB"))
  expect_identical(res$status, "conflict")
  expect_true(all(c("QCA", "QCB") %in% installed()))
})

test_that("codes are compared without regard to case", {
  local_eq_env()
  add3("QCA", 1); add3("QCB", 0.5)
  # The target differs only in case from an installed set: a conflict.
  res <- run_migrations(migration("QCA", "qcb"))
  expect_identical(res$status, "conflict")
  expect_true(all(c("QCA", "QCB") %in% installed()))
})

test_that("a rename that happens is recorded only once it is verified", {
  dir <- withr::local_tempdir()
  local_eq_env(dir)
  add3("QCA", 0.75)
  res <- run_migrations(migration("QCA", "QCC"))
  expect_identical(res$status, "ok")
  expect_identical(res$applied, "3L:QCA->QCC")
  expect_true("3L:QCA->QCC" %in% get_applied_migrations())
  expect_false("QCA" %in% installed())
  expect_equal(score("QCC"), 0.75)
})

test_that("nothing to rename, or already renamed, is applied", {
  local_eq_env()
  expect_identical(run_migrations(migration("QCX", "QCY"))$status, "ok")
  add3("QCZ", 1)
  res <- run_migrations(migration("QCW", "QCZ"))
  expect_identical(res$status, "ok")
  expect_identical(res$applied, "3L:QCW->QCZ")
})

test_that("a protected built-in is not renamed, and the migration stays pending", {
  local_eq_env()
  res <- run_migrations(migration("GB", "GB_NEW"))
  expect_identical(res$status, "failed")
  expect_identical(res$failed, "3L:GB->GB_NEW")
  expect_true("GB" %in% installed())
  expect_false("3L:GB->GB_NEW" %in% get_applied_migrations())
})

test_that("a rename whose save fails is not recorded", {
  local_eq_env()
  add3("QCA", 1)
  local_mocked_bindings(.save_cache = function(...) invisible(FALSE))
  res <- run_migrations(migration("QCA", "QCC"))
  expect_identical(res$status, "failed")
  expect_false("3L:QCA->QCC" %in% get_applied_migrations())
  expect_true("QCA" %in% installed())
})

# ── Q05 ───────────────────────────────────────────────────────────────────────

# A cache file that was rejected on load, holding `bytes`.
rejected_cache <- function(dir, bytes = charToRaw("unreadable sentinel")) {
  pkgenv <- local_eq_env(dir, .local_envir = parent.frame())
  path <- file.path(dir, .cache_basename)
  writeBin(bytes, path)
  assign(".cache_rejected", list(file = path, schema = 1L), envir = pkgenv)
  list(pkgenv = pkgenv, path = path, bytes = bytes)
}

test_that("a failed backup stops the save: bytes kept, nothing written", {
  dir <- withr::local_tempdir()
  rc <- rejected_cache(dir)
  local_mocked_bindings(.copy_backup = function(from, to) FALSE)
  expect_warning(ok <- .save_cache(rc$pkgenv, rc$path), "could not be backed up")
  expect_false(ok)
  expect_identical(readBin(rc$path, "raw", 1000L), rc$bytes)
  expect_identical(list.files(dir), .cache_basename)
  expect_true(exists(".cache_rejected", envir = rc$pkgenv, inherits = FALSE))
})

test_that("the review's injection -- the backup helper returning FALSE -- also stops it", {
  dir <- withr::local_tempdir()
  rc <- rejected_cache(dir)
  local_mocked_bindings(.backup_rejected_cache = function(...) invisible(FALSE))
  expect_warning(ok <- .save_cache(rc$pkgenv, rc$path))
  expect_false(ok)
  expect_identical(readBin(rc$path, "raw", 1000L), rc$bytes)
})

test_that("an add whose backup fails changes nothing, in the session or on disk", {
  dir <- withr::local_tempdir()
  rc <- rejected_cache(dir)
  local_mocked_bindings(.copy_backup = function(from, to) FALSE)
  expect_error(q(eqvs_add(vs3("QCA", 1), version = "3L", country = "x",
                          countryCode = "QC", VSCode = "QCA", saveOption = 3,
                          savePath = dir)))
  expect_false("QCA" %in% installed())
  expect_identical(readBin(rc$path, "raw", 1000L), rc$bytes)
})

test_that("a backup is kept, and a second never overwrites the first", {
  dir <- withr::local_tempdir()
  rc <- rejected_cache(dir, charToRaw("first"))
  expect_true(q(.save_cache(rc$pkgenv, rc$path)))
  baks <- list.files(dir, pattern = "\\.bak-")
  expect_length(baks, 1L)
  expect_identical(readBin(file.path(dir, baks), "raw", 100L), charToRaw("first"))
  # Rejected again the same day.
  writeBin(charToRaw("second"), rc$path)
  assign(".cache_rejected", list(file = rc$path, schema = 1L), envir = rc$pkgenv)
  expect_true(q(.save_cache(rc$pkgenv, rc$path)))
  baks2 <- sort(list.files(dir, pattern = "\\.bak-"))
  expect_length(baks2, 2L)
  contents <- vapply(file.path(dir, baks2), function(f)
    rawToChar(readBin(f, "raw", 100L)), "")
  expect_setequal(unname(contents), c("first", "second"))
})

test_that("no backup needed: the save simply succeeds", {
  dir <- withr::local_tempdir()
  pkgenv <- local_eq_env(dir)
  expect_true(q(.save_cache(pkgenv, file.path(dir, .cache_basename))))
  expect_length(list.files(dir, pattern = "\\.bak-"), 0L)
})

# ── Q11 ───────────────────────────────────────────────────────────────────────

empty_index <- function(v) {
  data.frame(Version = character(0), Name = character(0),
             Name_short = character(0), Country_code = character(0),
             VS_code = character(0), doi = character(0))
}

update_with <- function(migrations, index = empty_index, env = parent.frame()) {
  local_mocked_bindings(.has_internet = function() TRUE,
                        fetch_available_value_sets = index,
                        fetch_migrations = migrations,
                        check_builtin_conflicts = function(...) invisible(NULL),
                        .env = env)
  msgs <- character(0)
  res <- withCallingHandlers(update_value_sets(ask = FALSE),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m))
                            invokeRestart("muffleMessage") })
  list(res = res, msgs = paste(msgs, collapse = ""))
}

test_that("a failed migrations download is reported, and the date stays", {
  local_eq_env()
  set_last_checked(as.Date("2026-01-01"))
  out <- update_with(function() NULL)
  expect_true("migrations" %in% names(out$res$failed))
  expect_false(grepl("All value sets are up to date", out$msgs, fixed = TRUE))
  expect_identical(get_last_checked(), as.Date("2026-01-01"))
})

test_that("a successful empty migrations list still completes the check", {
  local_eq_env()
  set_last_checked(as.Date("2026-01-01"))
  out <- update_with(function() migration("A", "B")[0, ])
  expect_length(out$res$failed, 0L)
  expect_match(out$msgs, "All value sets are up to date", fixed = TRUE)
  expect_identical(get_last_checked(), Sys.Date())
})

test_that("a malformed migrations list is a failure", {
  local_eq_env()
  out <- update_with(function() data.frame(x = 1))
  expect_match(out$res$failed[["migrations"]], "malformed")
  expect_identical(get_last_checked(), as.Date("1970-01-01"))
})

test_that("an unresolved migration conflict leaves the check incomplete", {
  local_eq_env()
  add3("QCA", 1); add3("QCB", 0.5)
  set_last_checked(as.Date("2026-01-01"))
  out <- update_with(function() migration("QCA", "QCB"))
  expect_identical(out$res$migration_conflicts, "3L:QCA->QCB")
  expect_identical(get_last_checked(), as.Date("2026-01-01"))
  expect_false(grepl("All value sets are up to date", out$msgs, fixed = TRUE))
})

test_that("index and migrations failing together are both reported", {
  local_eq_env()
  out <- update_with(function() NULL, index = function(v) NULL)
  expect_setequal(names(out$res$failed), c("migrations", "3L", "5L", "Y3L"))
  expect_identical(get_last_checked(), as.Date("1970-01-01"))
})
