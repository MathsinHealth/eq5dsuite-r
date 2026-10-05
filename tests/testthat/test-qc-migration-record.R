# Verification of 2026-10-04, V02: a rename that was made but whose record
# could not be saved.
#
# The rename QCA -> QCB succeeded, but applied_migrations.rds could not be
# written (here: a directory of that name in the cache). set_applied_migrations()
# warned and returned NULL, nobody looked, and update_value_sets() reported
# "All value sets are up to date" and moved the last-checked date to today.
#
# Now the record is part of the migration phase: a rename that is made but not
# recorded is reported as such, the update is incomplete and the date stays.
# The renamed set is kept. On the next run the rename is not repeated -- the
# old code is gone -- and the record is written then.
#
# Every test runs on a temporary cache with mocked downloads.

q <- function(expr) suppressWarnings(suppressMessages(expr))

vs3 <- function(code, top) {
  s <- make_all_EQ_indexes("3L")
  df <- data.frame(state = s, v = seq(top, -0.5, length.out = length(s)))
  names(df)[2] <- code
  df
}

# Saved to the cache directory, so a reload finds it.
add3 <- function(code, top)
  q(eqvs_add(vs3(code, top), version = "3L", country = paste("Test", code),
             countryCode = substr(code, 1, 2), VSCode = code, saveOption = 2))

migration <- function(old, new, version = "3L")
  data.frame(version = version, old_VS_code = old, new_VS_code = new,
             reason = "test", date = "2026-10-04", stringsAsFactors = FALSE)

installed <- function(v = "3L") get_installed_vs_codes(v)
score <- function(code) q(eq5d(c(11111, 33333), country = code, version = "3L"))
record_path <- function() file.path(get_cache_dir(create = FALSE),
                                    "applied_migrations.rds")
empty_index <- function(v)
  data.frame(Version = character(0), Name = character(0),
             Name_short = character(0), Country_code = character(0),
             VS_code = character(0), doi = character(0))

update_with <- function(migrations, env = parent.frame()) {
  local_mocked_bindings(.has_internet = function() TRUE,
                        fetch_available_value_sets = empty_index,
                        fetch_migrations = function() migrations,
                        check_builtin_conflicts = function(...) invisible(NULL),
                        .env = env)
  msgs <- character(0); warns <- character(0)
  res <- withCallingHandlers(update_value_sets(ask = FALSE),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m))
                            invokeRestart("muffleMessage") },
    warning = function(w) { warns <<- c(warns, conditionMessage(w))
                            invokeRestart("muffleWarning") })
  list(res = res, msgs = paste(msgs, collapse = ""), warns = warns)
}

ID <- "3L:QCA->QCB"

# ── set_applied_migrations() says whether the record was saved ───────────────

test_that("set_applied_migrations() returns TRUE once saved, FALSE otherwise", {
  local_eq_env()
  expect_true(set_applied_migrations(ID))
  expect_identical(get_applied_migrations(), ID)
  unlink(record_path())
  dir.create(record_path())
  w <- testthat::capture_warnings(ok <- set_applied_migrations(ID))
  expect_true(any(grepl("Could not save applied migrations", w)))
  expect_false(ok)
})

# ── The report's reproduction: a directory where the record goes ─────────────

test_that("a blocked record: rename kept, phase failed, date kept, IDs not claimed", {
  dir <- withr::local_tempdir()
  local_eq_env(dir)
  add3("QCA", 0.9)
  before <- score("QCA")
  set_last_checked(as.Date("2026-01-01"))
  dir.create(record_path())

  out <- update_with(migration("QCA", "QCB"))
  # The rename happened and is kept, with its values.
  expect_false("QCA" %in% installed())
  expect_true("QCB" %in% installed())
  expect_identical(score("QCB"), before)
  # The failure is reported, as the migration phase.
  expect_match(out$res$failed[["migrations"]], "could not be recorded")
  expect_match(out$res$failed[["migrations"]], ID, fixed = TRUE)
  expect_identical(out$res$migrations_unrecorded, ID)
  expect_true(any(grepl("Could not save applied migrations", out$warns)))
  expect_false(grepl("All value sets are up to date", out$msgs, fixed = TRUE))
  expect_match(out$msgs, "record", fixed = TRUE)
  # The date stays, so the reminder is still due.
  expect_identical(get_last_checked(), as.Date("2026-01-01"))
  expect_false(ID %in% get_applied_migrations())

  # Reloaded from disk: the rename was saved; the record was not.
  local_eq_env(dir)
  q(eqvs_load(dir))
  expect_true("QCB" %in% installed())
  expect_false("QCA" %in% installed())
  expect_identical(score("QCB"), before)
  expect_false(ID %in% get_applied_migrations())

  # Still blocked: still reported, nothing renamed twice, date still kept.
  out2 <- update_with(migration("QCA", "QCB"))
  expect_match(out2$res$failed[["migrations"]], "could not be recorded")
  expect_identical(get_last_checked(), as.Date("2026-01-01"))

  # Unblocked: the retry records it, without renaming again, and completes.
  unlink(record_path(), recursive = TRUE)
  out3 <- update_with(migration("QCA", "QCB"))
  expect_length(out3$res$failed, 0L)
  expect_identical(out3$res$migrations_unrecorded, character(0))
  expect_identical(get_applied_migrations(), ID)
  expect_identical(get_last_checked(), Sys.Date())
  expect_match(out3$msgs, "All value sets are up to date", fixed = TRUE)
  expect_identical(score("QCB"), before)
})

# ── An injected save failure ─────────────────────────────────────────────────

test_that("an injected record failure is reported by both functions", {
  local_eq_env()
  add3("QCA", 0.9)
  set_last_checked(as.Date("2026-01-01"))
  local_mocked_bindings(set_applied_migrations = function(m) invisible(FALSE))

  local({
    local_mocked_bindings(fetch_migrations = function() migration("QCA", "QCB"))
    res <- q(apply_pending_migrations(ask = FALSE))
    expect_identical(res$status, "unrecorded")
    expect_identical(res$applied, ID)        # the rename was made...
    expect_identical(res$recorded, character(0))  # ...but not recorded
    expect_match(res$reason, "could not be recorded")
  })
  expect_true("QCB" %in% installed())

  out <- update_with(migration("QCA", "QCB"))
  expect_true("migrations" %in% names(out$res$failed))
  expect_identical(get_last_checked(), as.Date("2026-01-01"))
})

test_that("a successful record persists the IDs and completes the check", {
  local_eq_env()
  add3("QCA", 0.9)
  set_last_checked(as.Date("2026-01-01"))
  out <- update_with(migration("QCA", "QCB"))
  expect_length(out$res$failed, 0L)
  expect_identical(out$res$migrations_unrecorded, character(0))
  expect_identical(readRDS(record_path()), ID)
  expect_identical(get_last_checked(), Sys.Date())
})

# ── Retry is not an existing target taken as proof (Q04) ─────────────────────

test_that("on retry, the target existing does not count while the old code remains", {
  local_eq_env()
  # The record failed earlier, and since then a different QCA has appeared
  # beside the renamed QCB: that is a conflict, not a rename to record.
  add3("QCA", 0.9); add3("QCB", 0.5)
  out <- update_with(migration("QCA", "QCB"))
  expect_identical(out$res$migration_conflicts, ID)
  expect_false(ID %in% get_applied_migrations())
  expect_identical(score("QCA")[1], 0.9)
  expect_identical(score("QCB")[1], 0.5)
})
