# Findings of the external review of 15 October 2026
# (2026.10.15-eq5dsuite_astra_review.md, run on 9ae3fff). Its IDs clash with
# the earlier review's F01-F18 in PROGRESS.md, so this file and PROGRESS call
# them A01-A17 (findings) and AI01-AI08 (improvements). Each section has the
# review's own reproduction and the case it should have given.

q <- function(expr) suppressWarnings(suppressMessages(expr))
DIMS <- c("mo", "sc", "ua", "pd", "ad")

warnings_of <- function(expr) {
  w <- character(0)
  val <- withCallingHandlers(suppressMessages(expr), warning = function(e) {
    w <<- c(w, conditionMessage(e)); invokeRestart("muffleWarning") })
  list(value = val, warnings = w)
}

# ── A01. make_dummies() reads levels as the rest of the package does ──────────

test_that("A01: a factor level is read through its label, not its code", {
  d <- make_dummies(data.frame(mo = factor("3"), sc = 1, ua = 1, pd = 1, ad = 1), "3L")
  expect_identical(unname(unlist(d[1, c("mo2", "mo3")])), c(0, 1))
})

test_that("A01: numeric, text and factor forms of the same levels agree", {
  num <- data.frame(mo = c(1, 2, 3), sc = c(3, 2, 1), ua = 2, pd = 1, ad = 3)
  chr <- as.data.frame(lapply(num, as.character))
  fct <- as.data.frame(lapply(num, function(x) factor(x, levels = rev(sort(unique(x))))))
  ref <- make_dummies(num, "3L")
  expect_identical(make_dummies(chr, "3L"), ref)
  expect_identical(make_dummies(fct, "3L"), ref)
  expect_identical(make_dummies(as.matrix(num), "3L"), ref)
})

test_that("A01: an invalid level is an NA block with a warning, never a valid level", {
  for (bad in list(1.9, 0, -1, 4, Inf, 9)) {
    r <- warnings_of(make_dummies(
      data.frame(mo = c(2, bad), sc = 1, ua = 1, pd = 1, ad = 1), "3L"))
    expect_identical(nrow(r$value), 2L, info = format(bad))
    expect_identical(unname(unlist(r$value[1, c("mo2", "mo3")])), c(1, 0))
    expect_true(all(is.na(r$value[2, c("mo2", "mo3")])), info = format(bad))
    expect_false(anyNA(r$value[2, c("sc2", "sc3", "ad2", "ad3")]))
    expect_length(r$warnings, 1L)
  }
  # A missing level is an NA block too, without a warning.
  expect_no_warning(d <- make_dummies(
    data.frame(mo = NA, sc = 1, ua = 1, pd = 1, ad = 1), "3L"))
  expect_true(all(is.na(d[1, c("mo2", "mo3")])))
})

test_that("A01: shape is kept for one row, no rows and every option", {
  one <- data.frame(mo = 2, sc = 1, ua = 1, pd = 1, ad = 3)
  expect_identical(dim(make_dummies(one, "3L")), c(1L, 10L))
  expect_identical(dim(make_dummies(one[0, ], "3L")), c(0L, 10L))
  expect_identical(dim(make_dummies(one, "3L", add_intercept = TRUE)), c(1L, 11L))
  inc <- make_dummies(data.frame(mo = 3, sc = 1, ua = 1, pd = 1, ad = 1), "3L",
                      incremental = TRUE)
  expect_identical(unname(unlist(inc[1, c("mo2", "mo3")])), c(1, 1))
  expect_identical(dim(make_dummies(one, "3L", drop_level_1 = FALSE)), c(1L, 15L))
  expect_true(is.matrix(make_dummies(one, "3L", return_df = FALSE)))
  # Five-level data are unchanged.
  ref5 <- make_dummies(data.frame(mo = 5, sc = 4, ua = 3, pd = 2, ad = 1), "5L")
  expect_identical(unname(unlist(ref5[1, c("mo4", "mo5", "ad2")])), c(0, 1, 0))
})

test_that("A01: dim.names naming the same column twice is refused", {
  expect_error(make_dummies(data.frame(mo = 1, sc = 1, ua = 1, pd = 1, ad = 1), "3L",
                            dim.names = c("mo", "MO", "ua", "pd", "ad")),
               "more than once")
})

# ── A02. a saved cache is always one that can be read back ───────────────────

test_that("A02: .save_cache() refuses to replace the cache with one .read_cache() rejects", {
  dir <- withr::local_tempdir()
  pkgenv <- local_eq_env(dir)
  expect_no_error(q(eqvs_add(dummy_vs("3L", "QGOOD"), version = "3L",
                             VSCode = "QGOOD", saveOption = 2)))
  f <- file.path(dir, .cache_basename)
  before <- unname(tools::md5sum(f))
  # A value set that bypassed eqvs_add(), as a future bug might: the save
  # must not go through, and the good cache must stay readable.
  bad <- get("uservsets3L", envir = pkgenv)
  bad$QGOOD[1] <- Inf
  assign("uservsets3L", bad, envir = pkgenv)
  expect_warning(ok <- .save_cache(pkgenv, f), "could not write")
  expect_false(ok)
  expect_identical(unname(tools::md5sum(f)), before)
  expect_identical(.read_cache(f)$status, "ok")
})

# ── A10. a malformed schema number is rejected, never an error at load ───────

write_schema_cache <- function(schema) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  cache_schema_version <- schema
  save(cache_schema_version, file = file.path(dir, .cache_basename))
  dir
}

test_that("A10: every malformed schema number is a rejection", {
  for (sv in list(Inf, -Inf, NaN, 2.9, 0, -1, 1e12, .Machine$integer.max + 1,
                  "2", c(1, 2), TRUE, NULL)) {
    dir <- write_schema_cache(sv)
    res <- expect_no_error(.read_cache(file.path(dir, .cache_basename)))
    expect_identical(res$status, "reject", info = deparse(sv))
  }
  # Supported numbers are still read.
  for (sv in list(1, 2L, 2)) {
    dir <- write_schema_cache(sv)
    expect_false(identical(.read_cache(file.path(dir, .cache_basename))$status,
                           "reject"), info = deparse(sv))
  }
})

test_that("A10: loading with such a cache warns and goes on", {
  dir <- write_schema_cache(Inf)
  pkgenv <- new.env(parent = emptyenv())
  expect_warning(.apply_cache(pkgenv, dir), "could not be used")
  expect_true(exists(".cache_rejected", envir = pkgenv))
})

test_that("A10: a package loads in a fresh R process with a corrupt cache", {
  skip_on_cran()
  src <- testthat::test_path("..", "..")
  skip_if_not(file.exists(file.path(src, "DESCRIPTION")), "package sources not available")
  root <- withr::local_tempdir()
  cdir <- file.path(root, "R", "eq5dsuite")
  dir.create(cdir, recursive = TRUE)
  cache_schema_version <- Inf
  save(cache_schema_version, file = file.path(cdir, .cache_basename))
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("-e", shQuote(paste0(
      "suppressMessages(pkgload::load_all('", normalizePath(src),
      "', quiet = TRUE, export_all = FALSE)); cat('\\nSCORE', eq5d3l(11111, 'GB'))"))),
    env = c(paste0("R_USER_CACHE_DIR=", root), paste0("R_LIBS=", Sys.getenv("R_LIBS"))),
    stdout = TRUE, stderr = TRUE))
  expect_true(any(grepl("SCORE 1", out, fixed = TRUE)), info = paste(out, collapse = "\n"))
  expect_true(any(grepl("could not be used", out, fixed = TRUE)))
})

# ── A11. state codes are checked before they are converted ───────────────────

test_that("A11: fractional state codes are refused, and nothing is added", {
  local_eq_env()
  v <- dummy_vs("3L", "QFRAC"); v$state <- v$state + 0.9
  expect_error(q(eqvs_add(v, "3L", VSCode = "QFRAC")), "whole")
  expect_false("QFRAC" %in% get_installed_vs_codes("3L"))
  v <- dummy_vs("3L", "QFRAC1"); v$state[7] <- v$state[7] + 0.5
  expect_error(q(eqvs_add(v, "3L", VSCode = "QFRAC1")), "whole")
})

test_that("A11: duplicated and missing states are state-key errors", {
  local_eq_env()
  v <- dummy_vs("3L", "QDUP"); v$state[2] <- v$state[1]
  expect_error(q(eqvs_add(v, "3L", VSCode = "QDUP")), "exactly once")
  v <- dummy_vs("3L", "QMISS"); v$state[2] <- 99999
  expect_error(q(eqvs_add(v, "3L", VSCode = "QMISS")), "state")
})

test_that("A11: text and factor codes keep their meaning; shuffled rows are fine", {
  local_eq_env()
  v <- dummy_vs("3L", "QNUM")
  q(eqvs_add(v, "3L", VSCode = "QNUM"))
  ref <- q(eq5d3l(make_all_EQ_indexes("3L"), "QNUM"))
  refxw <- q(eqxw(make_all_EQ_indexes("5L")[1:50], "QNUM"))
  for (k in c("chr", "fct", "shuf")) {
    w <- dummy_vs("3L", paste0("Q", toupper(k)))
    if (k == "chr") w$state <- as.character(w$state)
    if (k == "fct") w$state <- factor(w$state, levels = rev(w$state))
    if (k == "shuf") w <- w[c(243:1), ]
    code <- names(w)[2]
    q(eqvs_add(w, "3L", VSCode = code))
    expect_identical(q(eq5d3l(make_all_EQ_indexes("3L"), code)), ref, info = k)
    expect_identical(q(eqxw(make_all_EQ_indexes("5L")[1:50], code)), refxw, info = k)
  }
})

test_that("A11: a cache with fractional state keys is rejected", {
  x <- data.frame(state = make_all_EQ_indexes("3L") + 0.5, A = 0.5)
  expect_match(.validate_uservsets(x, "3L"), "not health state codes")
})

# ── A12. dimension names are matched the same way everywhere ─────────────────

test_that("A12: explicit names work in every form and every scorer, in any case", {
  up <- c("MO", "SC", "UA", "PD", "AD")
  vals <- c(1, 2, 3, 1, 2)
  ref <- 12312L
  expect_identical(toEQ5Dindex(setNames(vals, up), dim.names = up), ref)
  expect_identical(toEQ5Dindex(setNames(vals, DIMS), dim.names = up), ref)
  expect_identical(toEQ5Dindex(setNames(vals, up)), ref)
  df_up <- as.data.frame(as.list(setNames(vals, up)))
  df_lo <- as.data.frame(as.list(setNames(vals, DIMS)))
  for (dn in list(up, DIMS)) for (d in list(df_up, df_lo, as.matrix(df_up))) {
    expect_identical(toEQ5Dindex(d, dim.names = dn), ref)
    expect_identical(q(eq5d(d, "US", "3L", dim.names = dn)), q(eq5d3l(ref, "US")))
    expect_identical(q(eqxw(d, "US", dim.names = dn)), q(eqxw(ref, "US")))
    expect_identical(q(eqxwr(d, "US", dim.names = dn)), q(eqxwr(ref, "US")))
  }
  # Custom names.
  cn <- c("Mobility", "SelfCare", "Usual", "Pain", "Anxiety")
  d <- as.data.frame(as.list(setNames(vals, cn)))
  expect_identical(q(eqxwr(d, "US", dim.names = tolower(cn))), q(eqxwr(ref, "US")))
  expect_identical(q(eq5d3l(d, "US", dim.names = toupper(cn))), q(eq5d3l(ref, "US")))
})

test_that("A12: columns that differ only in case are ambiguous, and say so", {
  d <- data.frame(mo = 1, MO = 2, sc = 1, ua = 1, pd = 1, ad = 1, check.names = FALSE)
  for (f in list(function(x) toEQ5Dindex(x), function(x) eq5d3l(x, "US"),
                 function(x) eqxw(x, "US"), function(x) eqxwr(x, "US")))
    expect_error(q(f(d)), "more than one column")
  expect_error(toEQ5Dindex(c(mo = 1, MO = 2, sc = 1, ua = 1, pd = 1, ad = 1)),
               "more than one")
})

test_that("A12: a name that is not there is still an error", {
  expect_error(q(eqxwr(data.frame(mo = 1, sc = 1, ua = 1, pd = 1, xx = 1), "US")),
               "ad")
  expect_error(toEQ5Dindex(c(mo = 1, sc = 1, ua = 1, pd = 1, xx = 1)), "ad")
})

# ── A13. the EQ VAS range is checked before rounding ──────────────────────────

test_that("A13: values outside 0-100 are NA even when they round inside", {
  x <- c(-0.4, -0.5, 100.4, 100.5, 0, 100, 50.5, 49.6, Inf, -Inf, NA)
  r <- warnings_of(.prep_vas(data.frame(v = x), "v"))
  expect_identical(r$value$vas, c(NA, NA, NA, NA, 0L, 100L, 50L, 50L, NA, NA, NA))
  # One warning for the rounding (2 values), one for the 6 out-of-range ones.
  expect_true(any(grepl("^2 VAS value", r$warnings)))
  expect_true(any(grepl("^6 VAS observation", r$warnings)))
})

test_that("A13: text and factor VAS values follow the same rule", {
  x <- c("-0.4", "100.4", "75", "75.2")
  ref <- q(.prep_vas(data.frame(v = as.numeric(x)), "v"))$vas
  expect_identical(q(.prep_vas(data.frame(v = x), "v"))$vas, ref)
  expect_identical(q(.prep_vas(data.frame(v = factor(x)), "v"))$vas, ref)
  expect_identical(ref, c(NA, NA, 75L, 75L))
})

test_that("A13: the summary counts what the Validation page reports as missing", {
  d <- data.frame(vas = c(-0.4, 100.4, 50, 60), fu = "a")
  s <- q(eq5d_vas_summary(d, name_vas = "vas", name_fu = "fu"))
  expect_identical(s[s$name == "Observations", 2], 2)
  v <- eq5d_validate(cbind(d, mo = 1, sc = 1, ua = 1, pd = 1, ad = 1),
                     list(names_eq5d = DIMS, eq5d_version = "3L", name_vas = "vas"),
                     quiet = TRUE)
  expect_true(any(grepl("2 ", v$message) & grepl("VAS", v$message)))
})

# ── A17. the instrument in a mapping is checked, not guessed ─────────────────

test_that("A17: an unsupported version is an error, not five levels", {
  d <- data.frame(mo = 4, sc = 1, ua = 1, pd = 1, ad = 1)
  for (v in c("4L", "Y5L", "")) {
    m <- list(names_eq5d = DIMS, eq5d_version = v)
    expect_error(eq5d_validate(d, m, quiet = TRUE), "Unknown EQ-5D version", info = v)
    expect_error(eq5d_apply_mapping(d, m), "Unknown EQ-5D version", info = v)
  }
  # Case variants are the instruments they name.
  for (v in c("3l", "y3l", "Y3L")) {
    v1 <- eq5d_validate(d, list(names_eq5d = DIMS, eq5d_version = v), quiet = TRUE)
    expect_true(any(grepl("from 1 to 3", v1$message)), info = v)
  }
  v5 <- eq5d_validate(d, list(names_eq5d = DIMS, eq5d_version = "5l"), quiet = TRUE)
  expect_true(any(grepl("within expected range (1–5)", v5$message, fixed = TRUE)))
  # No version at all still means "instrument not known".
  expect_no_error(eq5d_validate(d, list(names_eq5d = DIMS), quiet = TRUE))
})

# ── A03, A04. paired analyses need one record per respondent and timepoint, ──
# ── and, when split by group, the same group at every timepoint ─────────────

pair_fns <- list(
  pchc_table = function(d, g) eq5d_profile_pchc_table(d, name_id = "id",
    name_fu = "fu", levels_fu = c("a", "b", "c"), name_groupvar = g),
  pchc_noprob = function(d, g) eq5d_profile_pchc_with_no_problems_table(d,
    name_id = "id", name_fu = "fu", levels_fu = c("a", "b", "c"), name_groupvar = g),
  dim_change = function(d, g) eq5d_profile_dimension_change_table(d,
    name_id = "id", names_eq5d = DIMS, name_fu = "fu", levels_fu = c("a", "b", "c")),
  pchc_plot = function(d, g) eq5d_profile_pchc_by_group_plot(d, name_id = "id",
    name_fu = "fu", levels_fu = c("a", "b"), name_groupvar = g %||% "g"),
  better = function(d, g) eq5d_profile_better_dimensions_by_group_plot(d,
    name_id = "id", name_fu = "fu", levels_fu = c("a", "b"), name_groupvar = g %||% "g"),
  worse = function(d, g) eq5d_profile_worse_dimensions_by_group_plot(d,
    name_id = "id", name_fu = "fu", levels_fu = c("a", "b"), name_groupvar = g %||% "g"),
  mixed = function(d, g) eq5d_profile_mixed_dimensions_by_group_plot(d,
    name_id = "id", name_fu = "fu", levels_fu = c("a", "b"), name_groupvar = g %||% "g"),
  hpg = function(d, g) eq5d_profile_health_profile_grid(d, names_eq5d = DIMS,
    name_id = "id", name_fu = "fu", levels_fu = c("a", "b"),
    eq5d_version = "3L", country = "GB"))

pairs_data <- function() data.frame(
  mo = c(3, 2, 1, 2, 1, 3, 2), sc = c(1, 1, 1, 2, 1, 1, 2), ua = 1, pd = 2, ad = 1,
  id = c(1, 1, 1, 2, 2, 3, 3), fu = c("a", "b", "c", "a", "b", "a", "b"),
  g = c("Z", "Z", "Z", "Y", "Y", "Z", "Z"))

test_that("A03: a duplicated respondent-timepoint record stops every paired analysis", {
  d <- pairs_data()
  d <- rbind(d, d[2, ])                     # respondent 1 twice at "b"
  for (nm in names(pair_fns)) {
    expect_error(q(pair_fns[[nm]](d, NULL)), "more than one record", info = nm)
    expect_error(q(pair_fns[[nm]](d, NULL)), "id 1", info = nm)
  }
  # Duplicated at baseline, and duplicated across groups, too.
  d <- pairs_data(); d <- rbind(d, transform(d[1, ], mo = 1))
  expect_error(q(pair_fns$pchc_table(d, NULL)), "more than one record")
  d <- pairs_data(); d <- rbind(d, transform(d[2, ], g = "Y"))
  expect_error(q(pair_fns$pchc_table(d, "g")), "more than one record")
})

test_that("A03: the review's case no longer counts two changes for one respondent", {
  d <- data.frame(mo = c(3, 2, 1), sc = 1, ua = 1, pd = 1, ad = 1,
                  id = 1, fu = c("a", "b", "b"))
  expect_error(q(eq5d_profile_pchc_table(d, name_id = "id", name_fu = "fu",
                                         levels_fu = c("a", "b"))),
               "more than one record")
})

test_that("A03: records at a timepoint left out of levels_fu are not duplicates", {
  d <- pairs_data(); d <- rbind(d, transform(d[3, ], mo = 2))   # two at "c"
  r <- q(eq5d_profile_pchc_table(d, name_id = "id", name_fu = "fu",
                                 levels_fu = c("a", "b")))
  expect_s3_class(r, "data.frame")
})

test_that("A04: a respondent whose group changes stops the analyses split by group", {
  d <- data.frame(mo = c(3, 2, 1), sc = 1, ua = 1, pd = 1, ad = 1,
                  id = 1, fu = c("a", "b", "c"), g = c("Z", "A", "Z"))
  for (nm in setdiff(names(pair_fns), c("dim_change", "hpg")))
    expect_error(q(pair_fns[[nm]](d, "g")), "same group", info = nm)
  # A missing group at one visit is a change too.
  d$g <- c("Z", NA, "Z")
  expect_error(q(pair_fns$pchc_table(d, "g")), "same group")
  # Without a group the same data are fine.
  expect_no_error(q(pair_fns$pchc_table(d, NULL)))
})

test_that("A04: pairs follow time, not group, so row order and labels do not matter", {
  d <- pairs_data()
  ref <- q(pair_fns$pchc_table(d, "g"))
  expect_identical(q(pair_fns$pchc_table(d[c(7, 3, 5, 1, 6, 2, 4), ], "g")), ref)
  # Respondent 1 counts both changes, a->b and b->c.
  r <- q(pair_fns$pchc_table(d, NULL))
  expect_identical(r[r$state == "Grand Total", "All_b_n"], 3)
  expect_identical(r[r$state == "Grand Total", "All_c_n"], 1)
  # Renaming a group changes no counts.
  d2 <- d; d2$g[d2$g == "Y"] <- "AAA"
  r2 <- q(pair_fns$pchc_table(d2, "g"))
  expect_identical(unname(as.matrix(r2[, -1])), unname(as.matrix(ref[, -1]))[,
    match(sub("^AAA", "Y", names(r2)[-1]), names(ref)[-1])])
})

test_that("A03/A04 in the app: the Validation page says paired analyses will not run", {
  d <- eq5dsuite::example_data
  m <- list(names_eq5d = DIMS, eq5d_version = "3L", name_id = "id",
            name_fu = "time", name_groupvar = "procedure",
            levels_fu = c("Pre-op", "Post-op"))
  ok <- eq5d_validate(d, m, quiet = TRUE)
  expect_false(any(grepl("will not run", ok$message)))
  dup <- eq5d_validate(rbind(d, d[1, ]), m, quiet = TRUE)
  expect_true(any(grepl("duplicated", dup$message) & grepl("paired analyses will not run", dup$message)))
  d2 <- d; i <- which(d2$id == d2$id[1] & d2$time == "Post-op")
  d2$procedure[i] <- setdiff(unique(d2$procedure), d2$procedure[1])[1]
  mv <- eq5d_validate(d2, m, quiet = TRUE)
  hit <- grepl("same group at every timepoint", mv$message)
  expect_true(any(hit))
  expect_identical(mv$type[hit], "warning")
  # Without a group mapped there is nothing to say about groups.
  m$name_groupvar <- NULL
  expect_false(any(grepl("same group", eq5d_validate(d2, m, quiet = TRUE)$message)))
})

test_that("A03/A04 in the app: the analyses that cannot run say why", {
  skip_unless_app()
  e <- app_env()
  needs_of <- function(rv) {
    ids <- c("111", "122", "123", "124", "121fig", "122fig", "125fig")
    sp <- Filter(function(s) s$id %in% ids, e$ANALYSES)
    stats::setNames(lapply(sp, function(s) shiny::isolate(e$analysis_prerequisites(s, rv))),
                    vapply(sp, `[[`, "", "id"))
  }
  rv <- processed_rv(e)
  base <- needs_of(rv)
  expect_true(all(lengths(base) == 0L))

  d <- shiny::isolate(rv$processed_data)
  rv$processed_data <- rbind(d, d[1, ])
  dup <- needs_of(rv)
  expect_length(dup[["111"]], 0L)
  for (k in c("122", "123", "124", "121fig", "122fig", "125fig"))
    expect_true(any(grepl("one record per respondent", dup[[k]])), info = k)

  d2 <- d; i <- which(d2$id == d2$id[1] & d2$fu == "Post-op")
  d2$groupvar[i] <- setdiff(unique(d2$groupvar), d2$groupvar[1])[1]
  rv$processed_data <- d2
  mv <- needs_of(rv)
  for (k in c("122", "123", "121fig", "122fig"))
    expect_true(any(grepl("same group", mv[[k]])), info = k)
  for (k in c("111", "124", "125fig"))
    expect_length(mv[[k]], 0L)
})

# ── A05. the norms comparison summarises the baseline only ───────────────────

norms_data <- function() data.frame(
  utility = c(0.1, 0.9, 0.3, 0.5, 0.7), g = c("A", "B", "A", "B", "A"),
  age = c(30, 30, 50, 50, 30), fu = c("a", "b", "a", NA, "c"))

norms_cell <- function(r, group, stat, col = "Total")
  r[r$groupvar == group & r$name == stat, col]

test_that("A05: the review's case: an excluded timepoint does not contribute", {
  d <- data.frame(utility = c(0.1, 0.9), g = c("A", "B"), age = c(30, 30), fu = c("a", "b"))
  r <- q(eq5d_utility_norms_comparison(d, name_fu = "fu", levels_fu = "a",
                                       name_groupvar = "g", name_age = "age"))
  expect_identical(norms_cell(r, "Total", "n"), 1)
  expect_identical(norms_cell(r, "Total", "Mean"), 0.1)
  expect_false("B" %in% r$groupvar)
})

test_that("A05: only the first timepoint in levels_fu is summarised, and it says so", {
  m <- character(0)
  r <- withCallingHandlers(suppressWarnings(eq5d_utility_norms_comparison(
      norms_data(), name_fu = "fu", levels_fu = c("a", "b", "c"),
      name_groupvar = "g", name_age = "age")),
    message = function(e) { m <<- c(m, conditionMessage(e)); invokeRestart("muffleMessage") })
  expect_true(any(grepl("baseline", m) & grepl('"a"', m, fixed = TRUE)))
  expect_identical(norms_cell(r, "Total", "n"), 2)             # 0.1 and 0.3
  expect_equal(norms_cell(r, "Total", "Mean"), 0.2)
  expect_identical(norms_cell(r, "A", "n"), 2)
  expect_false("B" %in% r$groupvar)
  # Missing and unlisted timepoints never contribute; reordering the levels
  # moves the baseline.
  r2 <- q(eq5d_utility_norms_comparison(norms_data(), name_fu = "fu",
          levels_fu = c("b", "a"), name_groupvar = "g", name_age = "age"))
  expect_identical(norms_cell(r2, "Total", "n"), 1)
  expect_equal(norms_cell(r2, "Total", "Mean"), 0.9)
})

test_that("A05: the totals use exactly the cells' records", {
  d <- eq5dsuite::example_data
  d <- d[!is.na(d$ageband) & !is.na(d$gender), ]   # missing ones: see A06
  d$value <- q(eq5d3l(d[, DIMS], country = "GB"))
  d$ageband <- factor(d$ageband)
  r <- q(eq5d_utility_norms_comparison(d, name_utility = "value", name_fu = "time",
         levels_fu = c("Pre-op", "Post-op"), name_groupvar = "gender", name_age = "ageband"))
  pre <- d[d$time == "Pre-op", ]
  expect_identical(norms_cell(r, "Total", "n") + norms_cell(r, "Total", "Missing"),
                   as.numeric(nrow(pre)))
  expect_equal(norms_cell(r, "Total", "Mean"), mean(pre$value, na.rm = TRUE))
})

# ── A06. a missing group is not the group "NA" ───────────────────────────────

test_that("A06: the summary helpers keep NA, \"NA\" and separator strings apart", {
  d <- data.frame(groupvar = c(NA, "NA", "a\001b", "a", "b"), utility = c(0, 1, 0.2, 0.4, 0.6))
  for (f in list(.summary_table_4_3, .summary_table_4_4)) {
    r <- f(d, "groupvar")
    expect_identical(nrow(r), 5L)
    expect_true(all(r[[ncol(r) - 1L]] == 1))           # N / n: one record each
    expect_identical(sum(is.na(r$groupvar)), 1L)
  }
  r <- .summary_mean_ci(d, "groupvar")
  expect_identical(nrow(r), 5L)
  # Two columns whose pasted values coincide stay apart.
  d2 <- data.frame(a = c("x\001y", "x"), b = c("z", "y\001z"), utility = c(0, 1))
  expect_identical(nrow(.summary_table_4_3(d2, c("a", "b"))), 2L)
})

test_that("A06: the group order of the output is unchanged", {
  d <- data.frame(groupvar = c("b", "A", "a", "B", "b"), utility = 1:5 / 10)
  expect_identical(.summary_table_4_3(d, "groupvar")$groupvar,
                   sort(unique(d$groupvar)))
})

test_that("A06: by-group tables label a missing group, and keep it apart", {
  r <- q(eq5d_utility_summary_by_group(
    data.frame(utility = c(0, 1, 0.5), g = c(NA, "NA", "B")), name_groupvar = "g"))
  expect_true(all(c("B", "NA", "NA (missing)", "All groups") %in% names(r)))
  expect_identical(r[r$name == "N", "NA"], 1)
  expect_identical(r[r$name == "N", "NA (missing)"], 1)
  expect_identical(r[r$name == "N", "All groups"], 3)
  expect_false(anyDuplicated(names(r)) > 0)
})

test_that("A06: the norms comparison leaves out records without an age, and says so", {
  d <- data.frame(utility = c(0.1, 0.3, 0.5), g = c("A", NA, "NA"),
                  age = factor(c("20-29", NA, "20-29")), fu = "a")
  m <- character(0)
  r <- withCallingHandlers(suppressWarnings(eq5d_utility_norms_comparison(
      d, name_fu = "fu", name_groupvar = "g", name_age = "age")),
    message = function(e) { m <<- c(m, conditionMessage(e)); invokeRestart("muffleMessage") })
  expect_true(any(grepl("1 record", m) & grepl("age", m)))
  expect_identical(norms_cell(r, "Total", "n"), 2)
  expect_true(all(c("A", "NA") %in% r$groupvar))
  # A missing group is its own row.
  d$age <- factor("20-29")
  r <- q(eq5d_utility_norms_comparison(d, name_fu = "fu", name_groupvar = "g", name_age = "age"))
  expect_true(all(c("A", "NA", "NA (missing)", "Total") %in% r$groupvar))
  expect_identical(norms_cell(r, "Total", "n"), 3)
})

test_that("A06: the app's duplicate check keeps a missing ID apart from \"NA\"", {
  d <- data.frame(mo = 1, sc = 1, ua = 1, pd = 1, ad = 1, id = c("NA", NA), fu = "a")
  v <- eq5d_validate(d, list(names_eq5d = DIMS, eq5d_version = "3L",
                             name_id = "id", name_fu = "fu"), quiet = TRUE)
  expect_false(any(grepl("duplicated", v$message)))
})

# ── A07. a group with a total's name does not hide the total ─────────────────

test_that("A07: a group called \"All groups\" keeps its data, and the total is kept", {
  r <- q(eq5d_utility_summary_by_group(
    data.frame(utility = c(0.1, 0.9), g = c("All groups", "B")), name_groupvar = "g"))
  expect_false(anyDuplicated(names(r)) > 0)
  expect_identical(r[r$name == "N", "All groups"], 1)
  expect_equal(r[r$name == "Mean", "All groups"], 0.1)
  tot <- setdiff(names(r), c("name", "All groups", "B"))
  expect_length(tot, 1L)
  expect_match(tot, "total", ignore.case = TRUE)
  expect_equal(r[r$name == "Mean", tot], 0.5)
  expect_identical(r[r$name == "N", tot], 2)
})

test_that("A07: norms tables keep a group or age band called \"Total\" apart from the totals", {
  d <- data.frame(utility = c(0.1, 0.9, 0.5), g = c("Total", "B", "B"),
                  age = factor(c("Total", "20-29", "20-29")), fu = "a")
  r <- q(eq5d_utility_norms_comparison(d, name_fu = "fu", name_groupvar = "g", name_age = "age"))
  expect_false(anyDuplicated(names(r)) > 0)
  expect_identical(r[r$groupvar == "Total" & r$name == "n", "Total"], 1)
  tot_g <- setdiff(unique(r$groupvar), c("Total", "B"))
  tot_a <- setdiff(names(r), c("groupvar", "name", "Total", "20-29"))
  expect_length(tot_g, 1L); expect_length(tot_a, 1L)
  expect_identical(r[r$groupvar == tot_g & r$name == "n", tot_a], 3)
})

test_that("A07: the by-group plot keeps a group called \"All groups\"", {
  p <- q(eq5d_utility_by_group_plot(
    data.frame(utility = c(0.1, 0.9, 0.3), g = c("All groups", "B", "B")),
    name_groupvar = "g"))
  pd <- p$plot_data
  expect_identical(nrow(pd), 3L)
  expect_false(anyDuplicated(as.character(pd$groupvar)) > 0)
  expect_equal(pd$mean[as.character(pd$groupvar) == "All groups"], 0.1)
})

test_that("A06: the Validation page counts a missing ID and \"NA\" at one timepoint as different", {
  d <- data.frame(mo = 1, sc = 1, ua = 1, pd = 1, ad = 1,
                  id = c("NA", NA, "x", "x"), fu = c("a", "a", "a", "b"))
  v <- eq5d_validate(d, list(names_eq5d = DIMS, eq5d_version = "3L",
                             name_id = "id", name_fu = "fu"), quiet = TRUE)
  expect_false(any(grepl("duplicated", v$message)))
  d$id[2] <- "NA"
  v <- eq5d_validate(d, list(names_eq5d = DIMS, eq5d_version = "3L",
                             name_id = "id", name_fu = "fu"), quiet = TRUE)
  expect_true(any(grepl("^1 ID", v$message)))
})

# ── A08. an all-healthy cohort is a result, not an error ─────────────────────

noprob <- function(d, g = NULL) q(eq5d_profile_pchc_with_no_problems_table(
  d, name_id = "id", name_fu = "fu", levels_fu = c("a", "b"), name_groupvar = g))

test_that("A08: everyone without problems at both timepoints", {
  d <- data.frame(mo = 1, sc = 1, ua = 1, pd = 1, ad = 1, id = c(1, 1, 2, 2), fu = c("a", "b"))
  r <- noprob(d)
  expect_identical(r$state_noprobs, "No problems")
  expect_identical(r$All_b_n, 2)
  expect_identical(r$All_b_p, 1)
})

test_that("A08: one group entirely healthy beside one with problems", {
  d <- data.frame(mo = c(1, 1, 2, 1, 2, 2), sc = 1, ua = 1, pd = 1, ad = 1,
                  id = c(1, 1, 2, 2, 3, 3), fu = c("a", "b"),
                  g = c("H", "H", "P", "P", "P", "P"))
  r <- noprob(d, "g")
  row <- function(s) r[r$state_noprobs == s, ]
  expect_identical(row("No problems")$H_b_n, 1)
  expect_identical(row("No problems")$H_b_p, 1)
  expect_identical(row("Total with problems")$H_b_n, 0)
  expect_identical(row("Total with problems")$H_b_p, 0)
  # No denominator for the change categories in H: NA, not 0%.
  expect_true(is.na(row("Improve")$H_b_p))
  expect_identical(row("Improve")$H_b_n, 0)
  # P is as before.
  expect_identical(row("Improve")$P_b_n, 1)
  expect_identical(row("Improve")$P_b_p, 0.5)
  expect_identical(row("No change")$P_b_p, 0.5)
})

test_that("A08: everyone stable with problems, and a single paired respondent", {
  d <- data.frame(mo = 2, sc = 1, ua = 1, pd = 1, ad = 1, id = c(1, 1, 2, 2), fu = c("a", "b"))
  r <- noprob(d)
  expect_identical(r[r$state_noprobs == "No change", "All_b_p"], 1)
  d <- data.frame(mo = c(2, 1), sc = 1, ua = 1, pd = 1, ad = 1, id = 1, fu = c("a", "b"))
  expect_s3_class(noprob(d), "data.frame")
})

# ── A09. no complete profile is said in words, not by aggregate() ────────────

test_that("A09: analyses that need a complete profile say so when there is none", {
  d <- data.frame(mo = c(1, 2, 1, 3), sc = 1, ua = 1, pd = 1, ad = NA,
                  id = c(1, 1, 2, 2), fu = c("a", "b", "a", "b"), u = c(.5, .6, .7, .8))
  calls <- list(
    function() eq5d_profile_top_states(d, names_eq5d = DIMS, eq5d_version = "3L"),
    function() eq5d_profile_lfs_distribution(d, names_eq5d = DIMS, eq5d_version = "3L"),
    function() eq5d_profile_lfs_mean_utility(d, names_eq5d = DIMS, name_utility = "u", eq5d_version = "3L"),
    function() eq5d_profile_density_curve(d, DIMS, "3L"),
    function() eq5d_profile_lss_utility_plot(d, names_eq5d = DIMS, name_utility = "u", eq5d_version = "3L"),
    function() eq5d_profile_lfs_utility_plot(d, names_eq5d = DIMS, name_utility = "u", eq5d_version = "3L"),
    function() eq5d_profile_pchc_table(d, name_id = "id", names_eq5d = DIMS, name_fu = "fu", levels_fu = c("a", "b")),
    function() eq5d_profile_pchc_with_no_problems_table(d, name_id = "id", names_eq5d = DIMS, name_fu = "fu", levels_fu = c("a", "b")),
    function() eq5d_profile_pchc_by_group_plot(transform(d, g = "x"), name_id = "id", names_eq5d = DIMS,
                                               name_fu = "fu", levels_fu = c("a", "b"), name_groupvar = "g"))
  for (k in seq_along(calls)) {
    err <- tryCatch({ q(calls[[k]]()); NULL }, error = function(e) conditionMessage(e))
    expect_false(is.null(err), info = k)
    expect_match(err, "complete, valid EQ-5D profile", info = k)
    expect_false(grepl("aggregate|dimensions$|unary", err), info = k)
  }
  # The dimensions that are there are still summarised.
  expect_s3_class(q(eq5d_profile_level_summary(d, names_eq5d = DIMS, eq5d_version = "3L")),
                  "data.frame")
})

test_that("A09: zero rows, and filtering that empties a cohort, are reported the same way", {
  d0 <- data.frame(mo = numeric(0), sc = numeric(0), ua = numeric(0), pd = numeric(0), ad = numeric(0))
  expect_error(q(eq5d_profile_top_states(d0, names_eq5d = DIMS, eq5d_version = "3L")),
               "complete, valid EQ-5D profile|no rows|empty")
  d <- data.frame(mo = c(9, 9), sc = 1, ua = 1, pd = 1, ad = 1)
  expect_error(q(eq5d_profile_density_curve(d, DIMS, "3L")), "complete, valid EQ-5D profile")
})

# ── A14. the density curve starts at the origin ──────────────────────────────

test_that("A14: the plotted curve runs from (0, 0) to (1, 1); data and HSDI unchanged", {
  for (n_states in 1:3) {
    mo <- c(rep(1, 9), 2, 3)[seq_len(c(9, 10, 11)[n_states])]
    d <- data.frame(mo = mo, sc = 1, ua = 1, pd = 1, ad = 1)
    r <- q(eq5d_profile_density_curve(d, DIMS, "3L"))
    line <- ggplot2::ggplot_build(r$p)$data[[1]]
    expect_identical(c(line$x[1], line$y[1]), c(0, 0), info = n_states)
    expect_equal(c(utils::tail(line$x, 1), utils::tail(line$y, 1)), c(1, 1), info = n_states)
    expect_identical(nrow(r$plot_data), as.integer(n_states))
    expect_false(any(r$plot_data$Frequency == 0))
  }
  d <- data.frame(mo = c(rep(1, 9), 2), sc = 1, ua = 1, pd = 1, ad = 1)
  r <- q(eq5d_profile_density_curve(d, DIMS, "3L"))
  # HSDI: the area under (0,0), (0.9,0.5), (1,1), doubled.
  expect_equal(r$hsdi, 2 * (0.9 * 0.25 + 0.1 * 0.75))
})

# ── A15. a column called "follow-up" is found ────────────────────────────────

test_that("A15: eq5d_profile_change_summary() finds a column named follow-up", {
  d <- data.frame(mo = c(1, 2), sc = 1, ua = 1, pd = 1, ad = 1,
                  `follow-up` = c("a", "b"), check.names = FALSE)
  r <- q(eq5d_profile_change_summary(d, names_eq5d = DIMS, eq5d_version = "3L"))
  expect_true(any(grepl("_a$|^a_|_a_", names(r))))
  # "fu" takes precedence when both are there, and an explicit name wins.
  d$fu <- c("x", "y")
  r <- q(eq5d_profile_change_summary(d, names_eq5d = DIMS, eq5d_version = "3L"))
  expect_true(any(grepl("x", names(r))))
  d$visit <- c("p", "q")
  r <- q(eq5d_profile_change_summary(d, names_eq5d = DIMS, eq5d_version = "3L", name_fu = "visit"))
  expect_true(any(grepl("p", names(r))))
})

# ── A16. the deployment line in ?eq5d_app works ──────────────────────────────

test_that("A16: ?eq5d_app gives the three-colon call, and that call runs", {
  rd <- testthat::test_path("..", "..", "man", "eq5d_app.Rd")
  skip_if(!file.exists(rd), "package sources not available")
  txt <- readLines(rd)
  expect_false(any(grepl("eq5dsuite::eq5d_app(", txt, fixed = TRUE)))
  line <- grep("eq5dsuite:::eq5d_app(", txt, fixed = TRUE, value = TRUE)
  expect_length(line, 1L)
  skip_if_not_installed("shiny")
  # eq5d_app() sets options for the process on purpose; put every one back,
  # and remove the ones it added, or later tests run in online mode.
  op <- options()
  on.exit({
    added <- setdiff(names(options()), names(op))
    options(stats::setNames(rep(list(NULL), length(added)), added))
    options(op)
  }, add = TRUE)
  app <- eval(parse(text = trimws(line)), envir = globalenv())
  expect_s3_class(app, "shiny.appobj")
})

# ── AI02. update_value_sets(): inputs, success and the reminder date ─────────
# Everything is mocked; nothing reaches the network.

index_listing <- function(version, extra = character(0)) {
  codes <- c(get_installed_vs_codes(version), extra)
  data.frame(Version = version, Name = paste("Set", codes), Name_short = codes,
             Country_code = substr(codes, 1, 2), VS_code = codes, doi = "10.0/x",
             stringsAsFactors = FALSE)
}

update_with <- function(fetch, install = function(...) TRUE, versions = c("3L", "5L", "Y3L")) {
  local_mocked_bindings(
    .has_internet = function() TRUE,
    fetch_available_value_sets = fetch,
    install_value_set = install,
    apply_pending_migrations = function(...) invisible(list(status = "ok",
      applied = character(0), conflicts = character(0), failed = character(0), reason = NULL)),
    check_builtin_conflicts = function(...) invisible(character(0)))
  msgs <- character(0)
  res <- withCallingHandlers(update_value_sets(versions = versions, ask = FALSE),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  list(res = res, msgs = paste(msgs, collapse = ""))
}

test_that("AI02: versions are normalised, and an unknown one is an error", {
  local_eq_env()
  out <- update_with(function(v) index_listing(v), versions = c("5l", "y3L"))
  expect_setequal(out$res$checked, c("5L", "Y3L"))
  expect_error(update_with(function(v) index_listing(v), versions = "4L"), "Unknown EQ-5D version")
  expect_error(update_with(function(v) index_listing(v), versions = character(0)), "versions")
})

test_that("AI02: checking some instruments does not move the reminder date", {
  local_eq_env()
  set_last_checked(as.Date("2026-01-01"))
  out <- update_with(function(v) index_listing(v), versions = "5L")
  expect_identical(out$res$checked, "5L")
  expect_identical(get_last_checked(), as.Date("2026-01-01"))
  expect_match(out$msgs, "not updated")
  out <- update_with(function(v) index_listing(v))
  expect_identical(get_last_checked(), Sys.Date())
})

test_that("AI02: an installed code in another case is not new", {
  local_eq_env()
  installed <- NULL
  out <- update_with(function(v) {
    x <- index_listing(v)
    if (v == "3L") x$VS_code[x$VS_code == "GB"] <- "gb"
    x
  }, install = function(code, ...) { installed <<- c(installed, code); TRUE })
  expect_null(installed)
  expect_match(out$msgs, "up to date")
})

test_that("AI02: install_value_set() reports eqvs_add() refusing a code as a failure", {
  local_eq_env()
  q(eqvs_add(dummy_vs("3L", "QDUPE"), version = "3L", VSCode = "QDUPE"))
  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(state = make_all_EQ_indexes("3L"), value = 0.5),
                   csv, row.names = FALSE)
  local_mocked_bindings(.remote_csv = function(url) utils::read.csv(csv))
  meta <- data.frame(Name = "Dup", Country_code = "QD", doi = "10.0/x")
  ok <- q(install_value_set("qdupe", "3L", meta))
  expect_false(ok)
  ok <- q(install_value_set("QNEWSET", "3L", meta))
  expect_true(ok)
})

test_that("AI02: downloads go through one helper with time limits", {
  src <- testthat::test_path("..", "..", "R")
  skip_if(!dir.exists(src), "package sources not available")
  code <- unlist(lapply(list.files(src, "\\.R$", full.names = TRUE), readLines, warn = FALSE))
  expect_length(grep("curl::curl(", code, fixed = TRUE), 1L)
  body <- paste(deparse(.remote_csv), collapse = "\n")
  expect_match(body, "timeout")
})

# ── AI05. the XLSX download and its package ──────────────────────────────────

test_that("AI05: writexl is declared, and the XLSX button needs it", {
  desc <- testthat::test_path("..", "..", "DESCRIPTION")
  skip_if(!file.exists(desc), "package sources not available")
  sug <- read.dcf(desc, fields = "Suggests")[1, 1]
  expect_match(sug, "writexl")
  skip_unless_app()
  e <- app_env()
  rv <- processed_rv(e)
  html_with <- function(have) {
    e$has_package <- function(pkg) if (pkg == "writexl") have else TRUE
    out <- NULL
    q(shiny::testServer(e$mod_values_server, args = list(rv = rv), {
      out <<- as.character(output$download_ui$html)
    }))
    out
  }
  expect_match(html_with(TRUE), "download_xlsx", fixed = TRUE)
  without <- html_with(FALSE)
  expect_false(grepl("download_xlsx", without, fixed = TRUE))
  expect_match(without, "writexl", fixed = TRUE)
  expect_match(without, "download_csv", fixed = TRUE)
})

# ── AI06. non-finite values, intervals, and the PCHC figures' two timepoints ─

test_that("AI06: an infinite EQ-5D value is missing, with a warning; negatives are kept", {
  d <- data.frame(utility = c(-Inf, 0.5, Inf, -0.2, NaN), fu = "a")
  r <- warnings_of(eq5d_utility_summary(d, name_utility = "utility", name_fu = "fu"))
  s <- r$value
  expect_equal(s[s$name == "Mean", "a"], 0.15)
  expect_identical(s[s$name == "Observations", "a"], 2)
  expect_true(any(grepl("2 EQ-5D value", r$warnings) & grepl("finite", r$warnings)))
})

test_that("AI06: the interval plots document the normal approximation", {
  for (topic in c("eq5d_utility_over_time_plot", "eq5d_utility_by_group_plot",
                  "eq5d_utility_change_by_group_plot")) {
    rd <- testthat::test_path("..", "..", "man", paste0(topic, ".Rd"))
    skip_if(!file.exists(rd), "package sources not available")
    txt <- paste(readLines(rd), collapse = " ")
    expect_match(txt, "1.96", fixed = TRUE, info = topic)
    expect_match(txt, "normal approximation", fixed = TRUE, info = topic)
  }
})

test_that("AI06: the PCHC figures compare the first two timepoints only, and say so", {
  d <- data.frame(mo = c(3, 2, 1, 3, 3, 2), sc = 1, ua = 1, pd = 1, ad = 1,
                  id = rep(1:2, each = 3), fu = rep(c("a", "b", "c"), 2), g = "x")
  three <- function(f) {
    m <- character(0)
    r <- withCallingHandlers(suppressWarnings(f(d, name_id = "id", name_fu = "fu",
           levels_fu = c("a", "b", "c"), name_groupvar = "g")),
      message = function(e) { m <<- c(m, conditionMessage(e)); invokeRestart("muffleMessage") })
    list(r = r, m = m)
  }
  two <- function(f) q(f(d, name_id = "id", name_fu = "fu", levels_fu = c("a", "b"),
                         name_groupvar = "g"))
  for (f in list(eq5d_profile_pchc_by_group_plot, eq5d_profile_better_dimensions_by_group_plot)) {
    x <- three(f)
    expect_true(any(grepl('"a" and "b"', x$m, fixed = TRUE)))
    expect_equal(x$r$plot_data, two(f)$plot_data)
  }
  # Fewer than two timepoints is an error.
  expect_error(q(eq5d_profile_pchc_by_group_plot(d, name_id = "id", name_fu = "fu",
                 levels_fu = "a", name_groupvar = "g")), "two timepoints")
})

# ── AI07. the NICE mapping's bandwidth must be a finite, non-negative number ─

test_that("AI07: an infinite, negative or missing bandwidth is an error", {
  for (bw in list(Inf, -Inf, -1, NA_real_, NaN, c(0.1, Inf)))
    for (f in list(eqxw_UK, eqxwr_UK))
      expect_error(q(f(c(0.5, 0.6)[seq_along(bw)], age = 40, male = 1, bwidth = bw)),
                   "bwidth", info = deparse(bw))
  # The review's case: no longer a plausible-looking value.
  expect_error(q(eqxw_UK(99999, age = 40, male = 1, bwidth = Inf)), "bwidth")
})

test_that("AI07: valid bandwidths, and the DSU defaults, are unchanged", {
  expect_no_error(q(eqxw_UK(0.5, age = 40, male = 1, bwidth = 0)))
  expect_no_error(q(eqxw_UK(0.5, age = 40, male = 1, bwidth = 0.05)))
  expect_no_error(q(eqxwr_UK(c(0.5, 0.7), age = 40, male = 1, bwidth = c(0.1, 0.4))))
  expect_no_error(q(eqxw_UK(0.5, age = 40, male = 1, bwidth = "default")))
})

# ── Age-band columns of the norms comparison follow the factor's levels ──────
# (found while re-rendering the paper's Table S16, 2026-10-21: the only
# 20-29-year-old was a man, so "20 to 29" came after "80 to 89".)

test_that("norms comparison: age-band columns in factor-level order, total last", {
  bands <- c("20 to 29", "30 to 39", "40 to 49")
  d <- data.frame(utility = c(0.5, 0.6, 0.7, 0.8),
                  g = c("F", "F", "M", "M"),
                  age = factor(c("30 to 39", "40 to 49", "20 to 29", "40 to 49"), levels = bands),
                  fu = "a")
  r <- q(eq5d_utility_norms_comparison(d, name_fu = "fu", name_groupvar = "g", name_age = "age"))
  expect_identical(setdiff(names(r), c("groupvar", "name")), c(bands, "Total"))
  # A level nobody is in is not a column.
  d$age <- factor(as.character(d$age), levels = c("10 to 19", bands))
  r <- q(eq5d_utility_norms_comparison(d, name_fu = "fu", name_groupvar = "g", name_age = "age"))
  expect_identical(setdiff(names(r), c("groupvar", "name")), c(bands, "Total"))
  # Numeric ages: the bands in age order.
  d$age <- c(40, 50, 22, 50)
  r <- q(eq5d_utility_norms_comparison(d, name_fu = "fu", name_groupvar = "g", name_age = "age"))
  expect_identical(setdiff(names(r), c("groupvar", "name")),
                   c("18-24", "35-44", "45-54", "Total"))
})
