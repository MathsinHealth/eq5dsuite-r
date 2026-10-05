# The value set cache, and the operations that change it.
#
# Review findings F03, F11, F12, F13. Every test here runs against an isolated
# cache directory through local_eq_env(); none may touch the real user cache.
# get_cache_dir() now reads the package environment's cache_path, so
# last_checked.txt and applied_migrations.rds are redirected too.

q <- function(expr) suppressWarnings(suppressMessages(expr))

# A value set whose values are a known sequence, so a reordering shows up.
review_vs <- function(code, version = "3L") {
  # The EQ-5D-Y-3L shares the three-level state space. make_all_EQ_indexes()
  # accepts "Y3L" since F06 and returns the same states; "3L" is kept here.
  n <- if (version == "5L") 3125L else 243L
  states <- q(make_all_EQ_indexes(if (version == "5L") "5L" else "3L"))
  df <- data.frame(state = states, v = seq(1, 0, length.out = n))
  names(df)[2L] <- code
  df
}

user_codes <- function(version = "3L") {
  x <- getOption("eq.env")[[paste0("uservsets", toupper(version))]]
  setdiff(colnames(x), "state")
}

# Rewrite the cache file through `fn` and apply it to a fresh environment,
# the way .onLoad() does. Returns the validator's status.
reapply_cache <- function(dir, fn = identity) {
  f <- file.path(dir, "cache.Rdta")
  e <- new.env(); load(f, envir = e); fn(e)
  save(list = ls(e), envir = e, file = f)
  pkgenv <- new.env(parent = emptyenv())
  assign("cache_path", dir, envir = pkgenv)
  withr::local_options(list(eq.env = pkgenv), .local_envir = parent.frame())
  st <- q(.apply_cache(pkgenv, dir))
  q(.fixPkgEnv(saveCache = FALSE))
  st
}

# ---------------------------------------------------------------------------
# The cache directory is redirectable, so no test writes to the user's cache
# ---------------------------------------------------------------------------

test_that("get_cache_dir() follows the isolated cache", {
  real <- find_cache_dir("eq5dsuite")
  dir <- withr::local_tempdir()
  local_eq_env(cache_dir = dir)
  expect_identical(get_cache_dir(), dir)
  # And the files that used to go to the real cache whatever the test did.
  set_last_checked(as.Date("2020-01-01"))
  expect_true(file.exists(file.path(dir, "last_checked.txt")))
  expect_equal(get_last_checked(), as.Date("2020-01-01"))
  set_applied_migrations("review-1")
  expect_true(file.exists(file.path(dir, "applied_migrations.rds")))
  expect_false(identical(dir, real))
})

# ---------------------------------------------------------------------------
# F03: cache validation
# ---------------------------------------------------------------------------

test_that("permuting the cache rows leaves every output identical", {
  dir <- withr::local_tempdir()
  local_eq_env(cache_dir = dir)
  q(eqvs_add(review_vs("REVIEW_A"), version = "3L", country = "Review A",
             countryCode = "RVA", VSCode = "REVIEW_A", saveOption = 2))
  direct <- q(eq5d3l(c(11111, 12321, 33333), "REVIEW_A"))
  cross  <- q(eqxw(c(11111, 33333, 55555), "REVIEW_A"))
  expect_false(any(is.na(direct)))

  # Reversed, and randomly shuffled. The crosswalk multiplies the value set
  # by a probability matrix and so depends on row order: reversing the rows
  # used to reverse its output while leaving direct scoring alone.
  for (perm in list(rev, function(i) sample(i))) {
    st <- reapply_cache(dir, function(e) {
      i <- perm(seq_len(nrow(e$uservsets3L)))
      e$uservsets3L <- e$uservsets3L[i, , drop = FALSE]
    })
    expect_identical(st, "ok")
    expect_true("REVIEW_A" %in% user_codes())
    expect_equal(q(eq5d3l(c(11111, 12321, 33333), "REVIEW_A")), direct)
    expect_equal(q(eqxw(c(11111, 33333, 55555), "REVIEW_A")), cross)
  }
})

test_that("a cache with a broken state universe is rejected", {
  dir <- withr::local_tempdir()
  local_eq_env(cache_dir = dir)
  q(eqvs_add(review_vs("REVIEW_A"), version = "3L", country = "Review A",
             countryCode = "RVA", VSCode = "REVIEW_A", saveOption = 2))
  builtin <- q(eq5d3l(c(11111, 33333), "GB"))

  breakages <- list(
    "every key the same" = function(e) e$uservsets3L$state <- rep(11111L, 243L),
    "a duplicate row"    = function(e) e$uservsets3L <- rbind(e$uservsets3L,
                                                 e$uservsets3L[1L, , drop = FALSE]),
    "a key that is not a state" = function(e) e$uservsets3L$state[1L] <- 99999L,
    "a missing state"    = function(e) {
      e$uservsets3L$state[1L] <- e$uservsets3L$state[2L] },
    "a non-finite value" = function(e) e$uservsets3L$REVIEW_A[1L] <- Inf,
    "a key that is not a number" = function(e) {
      e$uservsets3L$state <- as.character(e$uservsets3L$state)
      e$uservsets3L$state[1L] <- "not a state" }
  )
  for (nm in names(breakages)) {
    st <- reapply_cache(dir, breakages[[nm]])
    expect_identical(st, "reject", info = nm)
    expect_false("REVIEW_A" %in% user_codes(), info = nm)
    # Rejecting the cache must not disturb the built-in value sets, which was
    # the serious half of this finding: a degenerate user key set collapsed
    # the join that builds the combined table and GB scored 1 for every state.
    expect_equal(q(eq5d3l(c(11111, 33333), "GB")), builtin, info = nm)
  }
})

test_that("a rejected cache is reported and the file left on disk", {
  dir <- withr::local_tempdir()
  local_eq_env(cache_dir = dir)
  q(eqvs_add(review_vs("REVIEW_A"), version = "3L", country = "Review A",
             countryCode = "RVA", VSCode = "REVIEW_A", saveOption = 2))
  f <- file.path(dir, "cache.Rdta")
  before <- readBin(f, "raw", file.size(f))
  pkgenv <- new.env(parent = emptyenv())
  assign("cache_path", dir, envir = pkgenv)
  withr::local_options(list(eq.env = pkgenv))
  e <- new.env(); load(f, envir = e)
  e$uservsets3L <- e$uservsets3L[1:100, , drop = FALSE]
  save(list = ls(e), envir = e, file = f)
  expect_warning(.apply_cache(pkgenv, dir), "could not be used")
})

# ---------------------------------------------------------------------------
# F12: one code-normalisation policy
# ---------------------------------------------------------------------------

test_that("a code differing only in case is refused, and the first survives", {
  local_eq_env()
  q(eqvs_add(review_vs("REVIEW_A"), version = "3L", country = "Review A",
             countryCode = "RVA", VSCode = "REVIEW_A", saveOption = 1))
  before <- q(eq5d3l(c(11111, 12321), "REVIEW_A"))

  expect_warning(
    eqvs_add(review_vs("review_a"), version = "3L", country = "Review a",
             countryCode = "RVa", VSCode = "review_a", saveOption = 1),
    "differing only in case")
  expect_identical(user_codes(), "REVIEW_A")
  expect_equal(q(eq5d3l(c(11111, 12321), "REVIEW_A")), before)
  # Both spellings resolve to the one set, rather than reporting an ambiguity
  # the user cannot act on.
  expect_equal(q(eq5d3l(11111, "review_a")), q(eq5d3l(11111, "REVIEW_A")))
  expect_equal(q(eq5d3l(11111, "Review_A")), q(eq5d3l(11111, "REVIEW_A")))
})

test_that("a built-in code is still refused in any case", {
  local_eq_env()
  for (code in c("GB", "gb", "Gb"))
    expect_error(q(eqvs_add(review_vs(code), version = "3L", country = "x",
                            countryCode = "x", VSCode = code, saveOption = 1)),
                 "already used by the built-in", info = code)
})

# ---------------------------------------------------------------------------
# F13: a failed operation changes nothing
# ---------------------------------------------------------------------------

test_that("eqvs_add() with an unusable save path changes nothing", {
  dir <- withr::local_tempdir()
  local_eq_env(cache_dir = dir)
  q(eqvs_add(review_vs("REVIEW_A"), version = "3L", country = "Review A",
             countryCode = "RVA", VSCode = "REVIEW_A", saveOption = 2))
  f <- file.path(dir, "cache.Rdta")
  before_file <- readBin(f, "raw", file.size(f))
  before_meta <- getOption("eq.env")$user_defined_3L

  expect_error(q(eqvs_add(review_vs("REVIEW_BAD"), version = "3L",
                          country = "Bad", countryCode = "RVB",
                          VSCode = "REVIEW_BAD", saveOption = 3,
                          savePath = file.path(tempdir(), "no-such-review-dir"))),
               "savePath")

  # The registry used to advertise REVIEW_BAD with no values behind it, and
  # the next lookup failed with "replacement has length zero".
  expect_identical(user_codes(), "REVIEW_A")
  expect_false("REVIEW_BAD" %in% before_meta$VS_code)
  expect_identical(getOption("eq.env")$user_defined_3L$VS_code, "REVIEW_A")
  expect_false(is.na(q(eq5d3l(11111, "REVIEW_A"))))
  expect_error(q(eq5d3l(11111, "REVIEW_BAD")), "No valid countries")
  # And the cache on disk is untouched.
  expect_identical(readBin(f, "raw", file.size(f)), before_file)
})

test_that("eqvs_drop() with an unusable save path removes nothing", {
  dir <- withr::local_tempdir()
  local_eq_env(cache_dir = dir)
  q(eqvs_add(review_vs("REVIEW_A"), version = "3L", country = "Review A",
             countryCode = "RVA", VSCode = "REVIEW_A", saveOption = 2))
  before <- q(eq5d3l(c(11111, 12321), "REVIEW_A"))

  expect_error(q(eqvs_drop(country = "REVIEW_A", version = "3L",
                           saveOption = 3, ask = FALSE,
                           savePath = file.path(tempdir(), "no-such-review-dir"))),
               "savePath")
  # The removal used to happen first, so the session no longer matched disk.
  expect_identical(user_codes(), "REVIEW_A")
  expect_equal(q(eq5d3l(c(11111, 12321), "REVIEW_A")), before)
})

# ---------------------------------------------------------------------------
# F11: rename
# ---------------------------------------------------------------------------

test_that("a user-defined rename preserves every value, and persists", {
  dir <- withr::local_tempdir()
  local_eq_env(cache_dir = dir)
  q(eqvs_add(review_vs("REVIEW_A"), version = "3L", country = "Review A",
             countryCode = "RVA", VSCode = "REVIEW_A", saveOption = 2))
  direct <- q(eq5d3l(c(11111, 12321, 33333), "REVIEW_A"))
  cross  <- q(eqxw(c(11111, 33333, 55555), "REVIEW_A"))

  expect_true(q(rename_value_set("REVIEW_A", "REVIEW_B", "3L", ask = FALSE)))
  expect_identical(user_codes(), "REVIEW_B")
  expect_equal(unname(q(eq5d3l(c(11111, 12321, 33333), "REVIEW_B"))),
               unname(direct))
  expect_equal(unname(q(eqxw(c(11111, 33333, 55555), "REVIEW_B"))),
               unname(cross))
  expect_error(q(eq5d3l(11111, "REVIEW_A")), "No valid countries")

  # And it survives being read back from the cache.
  pkgenv <- new.env(parent = emptyenv())
  assign("cache_path", dir, envir = pkgenv)
  withr::local_options(list(eq.env = pkgenv))
  q(.apply_cache(pkgenv, dir))
  q(.fixPkgEnv(saveCache = FALSE))
  expect_identical(user_codes(), "REVIEW_B")
  expect_equal(unname(q(eq5d3l(c(11111, 12321, 33333), "REVIEW_B"))),
               unname(direct))
})

test_that("a built-in rename is refused rather than reported as done", {
  local_eq_env()
  before <- q(eq5d3l(c(11111, 33333), "GB"))
  expect_message(r <- rename_value_set("GB", "GB_NEW", "3L", ask = FALSE),
                 "built-in value set")
  expect_false(r)
  # Nothing moved, and the old code still works.
  expect_equal(q(eq5d3l(c(11111, 33333), "GB")), before)
  expect_error(q(eq5d3l(11111, "GB_NEW")), "No valid countries")
  expect_true("GB" %in% get_installed_vs_codes("3L"))
})

test_that("a rename onto an existing code leaves both sets intact", {
  local_eq_env()
  for (code in c("REVIEW_A", "REVIEW_C"))
    q(eqvs_add(review_vs(code), version = "3L", country = code,
               countryCode = code, VSCode = code, saveOption = 1))
  a <- q(eq5d3l(11111, "REVIEW_A"))

  expect_false(q(rename_value_set("REVIEW_A", "REVIEW_C", "3L", ask = FALSE)))
  # Including a target that differs only in case.
  expect_false(q(rename_value_set("REVIEW_A", "review_c", "3L", ask = FALSE)))
  # And onto a built-in code.
  expect_false(q(rename_value_set("REVIEW_A", "GB", "3L", ask = FALSE)))
  expect_setequal(user_codes(), c("REVIEW_A", "REVIEW_C"))
  expect_equal(q(eq5d3l(11111, "REVIEW_A")), a)
})

test_that("a failed write during a rename restores the session", {
  dir <- withr::local_tempdir()
  local_eq_env(cache_dir = dir)
  q(eqvs_add(review_vs("REVIEW_A"), version = "3L", country = "Review A",
             countryCode = "RVA", VSCode = "REVIEW_A", saveOption = 2))
  before <- q(eq5d3l(c(11111, 12321), "REVIEW_A"))

  # .fixPkgEnv() cannot write, so the rename must roll back.
  testthat::local_mocked_bindings(
    .fixPkgEnv = function(saveCache = FALSE, ...) if (isTRUE(saveCache)) FALSE else TRUE,
    .package = "eq5dsuite")
  expect_message(r <- rename_value_set("REVIEW_A", "REVIEW_B", "3L", ask = FALSE),
                 "Nothing has been changed")
  expect_false(r)
  expect_identical(user_codes(), "REVIEW_A")
})

test_that("a rename migration is not recorded when the rename fails", {
  # migrations.R records only on success, which is already correct; this
  # pins it, because a migration wrongly marked applied never runs again.
  local_eq_env()
  expect_false(q(rename_value_set("GB", "GB_NEW", "3L", ask = FALSE)))
  expect_length(q(get_applied_migrations()), 0L)
})

# ---------------------------------------------------------------------------
# Injected write failures, not only unusable paths
# ---------------------------------------------------------------------------

# The whole of the user's value set state, for comparing before and after.
vs_state <- function() {
  pkgenv <- getOption("eq.env")
  nms <- .cache_user_objects(pkgenv)
  stats::setNames(lapply(nms, function(n) get(n, envir = pkgenv)), nms)
}

test_that("an add whose write fails changes nothing, in memory or on disk", {
  dir <- withr::local_tempdir()
  local_eq_env(cache_dir = dir)
  q(eqvs_add(review_vs("REVIEW_A"), version = "3L", country = "Review A",
             countryCode = "RVA", VSCode = "REVIEW_A", saveOption = 2))
  f <- file.path(dir, "cache.Rdta")
  before_file  <- readBin(f, "raw", file.size(f))
  before_state <- vs_state()
  before_score <- q(eq5d3l(c(11111, 12321, 33333), "REVIEW_A"))

  # The write itself fails, rather than the path being wrong.
  testthat::local_mocked_bindings(
    .save_cache = function(...) invisible(FALSE), .package = "eq5dsuite")
  expect_error(q(eqvs_add(review_vs("REVIEW_D"), version = "3L",
                          country = "Review D", countryCode = "RVD",
                          VSCode = "REVIEW_D", saveOption = 2)),
               "Nothing has been changed")

  expect_identical(user_codes(), "REVIEW_A")
  expect_equal(vs_state(), before_state)
  expect_equal(q(eq5d3l(c(11111, 12321, 33333), "REVIEW_A")), before_score)
  expect_error(q(eq5d3l(11111, "REVIEW_D")), "No valid countries")
  expect_identical(readBin(f, "raw", file.size(f)), before_file)
})

test_that("a drop whose write fails removes nothing", {
  dir <- withr::local_tempdir()
  local_eq_env(cache_dir = dir)
  for (code in c("REVIEW_A", "REVIEW_C"))
    q(eqvs_add(review_vs(code), version = "3L", country = code,
               countryCode = code, VSCode = code, saveOption = 2))
  f <- file.path(dir, "cache.Rdta")
  before_file  <- readBin(f, "raw", file.size(f))
  before_state <- vs_state()
  before_score <- q(eq5d3l(c(11111, 12321), "REVIEW_A"))

  testthat::local_mocked_bindings(
    .save_cache = function(...) invisible(FALSE), .package = "eq5dsuite")
  expect_error(q(eqvs_drop(country = "REVIEW_A", version = "3L",
                           saveOption = 2, ask = FALSE)),
               "Nothing has been changed")

  expect_setequal(user_codes(), c("REVIEW_A", "REVIEW_C"))
  expect_equal(vs_state(), before_state)
  expect_equal(q(eq5d3l(c(11111, 12321), "REVIEW_A")), before_score)
  expect_identical(readBin(f, "raw", file.size(f)), before_file)
})

test_that("a rename whose write fails restores metadata and scoring too", {
  dir <- withr::local_tempdir()
  local_eq_env(cache_dir = dir)
  q(eqvs_add(review_vs("REVIEW_A"), version = "3L", country = "Review A",
             countryCode = "RVA", VSCode = "REVIEW_A", saveOption = 2))
  f <- file.path(dir, "cache.Rdta")
  before_file  <- readBin(f, "raw", file.size(f))
  before_state <- vs_state()
  before_score <- q(eq5d3l(c(11111, 12321, 33333), "REVIEW_A"))
  before_cross <- q(eqxw(c(11111, 55555), "REVIEW_A"))

  testthat::local_mocked_bindings(
    .save_cache = function(...) invisible(FALSE), .package = "eq5dsuite")
  expect_message(r <- rename_value_set("REVIEW_A", "REVIEW_B", "3L", ask = FALSE),
                 "Nothing has been changed")
  expect_false(r)

  expect_identical(user_codes(), "REVIEW_A")
  expect_equal(vs_state(), before_state)
  expect_equal(q(eq5d3l(c(11111, 12321, 33333), "REVIEW_A")), before_score)
  expect_equal(q(eqxw(c(11111, 55555), "REVIEW_A")), before_cross)
  expect_error(q(eq5d3l(11111, "REVIEW_B")), "No valid countries")
  expect_identical(readBin(f, "raw", file.size(f)), before_file)
})

# ---------------------------------------------------------------------------
# Atomic replacement: a failed write keeps the previous usable file
# ---------------------------------------------------------------------------

test_that("a cache write that fails part way leaves the old file usable", {
  dir <- withr::local_tempdir()
  local_eq_env(cache_dir = dir)
  q(eqvs_add(review_vs("REVIEW_A"), version = "3L", country = "Review A",
             countryCode = "RVA", VSCode = "REVIEW_A", saveOption = 2))
  f <- file.path(dir, "cache.Rdta")
  before_file <- readBin(f, "raw", file.size(f))

  # save() to a new file succeeds; moving it into place does not. This is the
  # moment that used to destroy the previous cache, because save() wrote
  # straight to the target and truncated it first.
  testthat::local_mocked_bindings(
    file.rename = function(from, to) FALSE, .package = "base")
  expect_warning(ok <- .save_cache(getOption("eq.env"), f),
                 "could not be moved into place")
  expect_false(ok)

  # Byte-identical, and still loadable.
  expect_identical(readBin(f, "raw", file.size(f)), before_file)
  probe <- new.env(); load(f, envir = probe)
  expect_true("uservsets3L" %in% ls(probe))
  expect_true("REVIEW_A" %in% colnames(probe$uservsets3L))
  # And no temporary file is left beside it.
  expect_setequal(list.files(dir), "cache.Rdta")
})

test_that("a cache that cannot be read back is not put into place", {
  dir <- withr::local_tempdir()
  local_eq_env(cache_dir = dir)
  q(eqvs_add(review_vs("REVIEW_A"), version = "3L", country = "Review A",
             countryCode = "RVA", VSCode = "REVIEW_A", saveOption = 2))
  f <- file.path(dir, "cache.Rdta")
  before_file <- readBin(f, "raw", file.size(f))

  testthat::local_mocked_bindings(
    save = function(...) invisible(NULL), .package = "base")
  expect_warning(ok <- .save_cache(getOption("eq.env"), f), "could not write")
  expect_false(ok)
  expect_identical(readBin(f, "raw", file.size(f)), before_file)
})

# ---------------------------------------------------------------------------
# Every instrument, and the legacy cache schema
# ---------------------------------------------------------------------------

test_that("the state universe is checked for 3L, 5L and Y3L alike", {
  for (spec in list(list(v = "3L",  obj = "uservsets3L",  n = 243L),
                    list(v = "5L",  obj = "uservsets5L",  n = 3125L),
                    list(v = "Y3L", obj = "uservsetsY3L", n = 243L))) {
    dir <- withr::local_tempdir()
    local_eq_env(cache_dir = dir)
    code <- paste0("REVIEW_", spec$v)
    q(eqvs_add(review_vs(code, version = spec$v), version = spec$v,
               country = code, countryCode = "RV", VSCode = code,
               saveOption = 2))
    expect_true(code %in% user_codes(spec$v), info = spec$v)

    # Accepted when merely reordered, and the values follow the states.
    st <- reapply_cache(dir, function(e) {
      i <- rev(seq_len(nrow(e[[spec$obj]])))
      e[[spec$obj]] <- e[[spec$obj]][i, , drop = FALSE]
    })
    expect_identical(st, "ok", info = spec$v)
    expect_true(code %in% user_codes(spec$v), info = spec$v)

    # Rejected when the universe is broken, with built-ins intact.
    st <- reapply_cache(dir, function(e)
      e[[spec$obj]]$state <- rep(e[[spec$obj]]$state[1L], spec$n))
    expect_identical(st, "reject", info = spec$v)
    expect_false(code %in% user_codes(spec$v), info = spec$v)
    expect_false(is.na(q(eq5d3l(11111, "GB"))), info = spec$v)
  }
})

test_that("a legacy cache is migrated, validated, and canonicalised", {
  for (spec in list(list(v = "3L",  obj = "uservsets3L",  meta = "user_defined_3L"),
                    list(v = "5L",  obj = "uservsets5L",  meta = "user_defined_5L"),
                    list(v = "Y3L", obj = "uservsetsY3L", meta = "user_defined_Y3L"))) {
    dir <- withr::local_tempdir()
    local_eq_env(cache_dir = dir)
    code <- paste0("LEGACY_", spec$v)
    q(eqvs_add(review_vs(code, version = spec$v), version = spec$v,
               country = code, countryCode = "LG", VSCode = code,
               saveOption = 2))
    want <- q(eq5d(11111, country = code,
                   version = if (spec$v == "Y3L") "Y3L" else spec$v))

    # Schema 1 is a cache with no version stamp. Reordered as well, so the
    # migration and the canonicalisation are both exercised.
    st <- reapply_cache(dir, function(e) {
      if (exists("cache_schema_version", envir = e)) rm("cache_schema_version", envir = e)
      i <- rev(seq_len(nrow(e[[spec$obj]])))
      e[[spec$obj]] <- e[[spec$obj]][i, , drop = FALSE]
    })
    expect_true(st %in% c("ok", "migrated"), info = spec$v)
    expect_true(code %in% user_codes(spec$v), info = spec$v)
    expect_equal(q(eq5d(11111, country = code,
                        version = if (spec$v == "Y3L") "Y3L" else spec$v)),
                 want, info = spec$v)

    # A legacy cache with a broken universe is still rejected.
    st <- reapply_cache(dir, function(e) {
      if (exists("cache_schema_version", envir = e)) rm("cache_schema_version", envir = e)
      e[[spec$obj]] <- e[[spec$obj]][-1L, , drop = FALSE]
    })
    expect_identical(st, "reject", info = spec$v)
    expect_false(code %in% user_codes(spec$v), info = spec$v)
  }
})
