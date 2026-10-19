# Findings from checking the R Journal manuscript against the package
# (October 2026).
#
# 1. eqvs_add() accepted infinite values. The state scored Inf for the rest of
#    the session, and a set saved to the cache was dropped when the package was
#    next loaded, because the cache check refuses non-finite values.
# 2. bwidth = "default" for mean values must replicate the DSU's own R command
#    exactly, including at 0.6, where NICE's FAQ wording differs.
# 3. eq5d_profile_level_summary(), which takes no time variable, printed
#    "No ordering of time suppled" -- irrelevant there, and misspelt.
# 4. The value functions returned NA without a word for a state outside the
#    instrument: mo = 4 or a missing-value code 9 in the EQ-5D-3L.
# 5. toEQ5Ddims() returned double columns; make_all_EQ_states() returns
#    integer ones, and the two are meant to have the same layout.
# 6. ?eq5d3l, ?eq5d and the other value functions did not say how missing and
#    invalid responses are handled.
# 7. ?eqvs_add did not describe its checks, and said it returned TRUE/FALSE.
# 8. ?example_data described year as a plain year, and said only the analysis
#    functions treat the 9 and 999 codes as missing.

q <- function(expr) suppressWarnings(suppressMessages(expr))
DIMS <- c("mo", "sc", "ua", "pd", "ad")

# ── 1. eqvs_add() and non-finite values ───────────────────────────────────────

vs3 <- function(code) {
  s <- make_all_EQ_indexes("3L")
  df <- data.frame(state = s, v = seq(1, -0.5, length.out = length(s)))
  names(df)[2] <- code
  df
}

test_that("eqvs_add() refuses infinite values, and adds nothing", {
  local_eq_env()
  for (bad in c(Inf, -Inf)) {
    df <- vs3("QCINF"); df$QCINF[5] <- bad
    expect_error(q(eqvs_add(df, version = "3L", country = "T", VSCode = "QCINF",
                            saveOption = 1)),
                 "not finite", info = format(bad))
    expect_false("QCINF" %in% get_installed_vs_codes("3L"))
  }
  # NaN and NA are still refused as before.
  df <- vs3("QCNAN"); df$QCNAN[5] <- NaN
  expect_error(q(eqvs_add(df, version = "3L", country = "T", VSCode = "QCNAN",
                          saveOption = 1)))
  # A finite set is still accepted.
  expect_no_error(q(eqvs_add(vs3("QCOK"), version = "3L", country = "T",
                             VSCode = "QCOK", saveOption = 1)))
  expect_equal(q(eq5d3l(11111, country = "QCOK")), 1)
})

test_that("nothing infinite reaches the cache", {
  dir <- withr::local_tempdir()
  local_eq_env(dir)
  df <- vs3("QCINF"); df$QCINF[5] <- Inf
  expect_error(q(eqvs_add(df, version = "3L", country = "T", VSCode = "QCINF",
                          saveOption = 2)), "not finite")
  expect_false(file.exists(file.path(dir, .cache_basename)))
})

# ── 2. bwidth = "default" replicates the DSU's R command ──────────────────────

# Computed with fun_map() from the DSU's R command for mapping between the
# EQ-5D-3L and EQ-5D-5L (eq5d_mapping-functions.R, April 2026 release), one
# row per call as that command requires, with bwidth = "default" and
# country = c("UK", "UK"). Its rule is if_else(x > 0.6, 0.1, 0.4), so a mean
# of exactly 0.6 takes the bandwidth 0.4. (NICE's PMG51 FAQ describes 0.6 as
# within the 0.1 range; the package follows the DSU's command.) The 3L->5L
# values agree to about 3e-8: the DSU's table stores the Dolan (1997) values
# rounded, the package at full precision.
dsu_default_bw <- list(
  `5L` = rbind(
    c(u = 0.5999, age = 30, male = 0, dsu = 0.7435470260),
    c(u = 0.6000, age = 30, male = 0, dsu = 0.7436113223),
    c(u = 0.6001, age = 30, male = 0, dsu = 0.7493152835),
    c(u = 0.5999, age = 70, male = 0, dsu = 0.7338445065),
    c(u = 0.6000, age = 70, male = 0, dsu = 0.7339127968),
    c(u = 0.6001, age = 70, male = 0, dsu = 0.7371499315),
    c(u = 0.5999, age = 30, male = 1, dsu = 0.7499901095),
    c(u = 0.6000, age = 30, male = 1, dsu = 0.7500553053),
    c(u = 0.6001, age = 30, male = 1, dsu = 0.7585422401),
    c(u = 0.5999, age = 70, male = 1, dsu = 0.7374938292),
    c(u = 0.6000, age = 70, male = 1, dsu = 0.7375639079),
    c(u = 0.6001, age = 70, male = 1, dsu = 0.7454367003)),
  `3L` = rbind(
    c(u = 0.5999, age = 30, male = 0, dsu = 0.4395527820),
    c(u = 0.6000, age = 30, male = 0, dsu = 0.4396175738),
    c(u = 0.6001, age = 30, male = 0, dsu = 0.5134785813),
    c(u = 0.5999, age = 70, male = 0, dsu = 0.4271397682),
    c(u = 0.6000, age = 70, male = 0, dsu = 0.4272116351),
    c(u = 0.6001, age = 70, male = 0, dsu = 0.5128489013),
    c(u = 0.5999, age = 30, male = 1, dsu = 0.4469744042),
    c(u = 0.6000, age = 30, male = 1, dsu = 0.4470383916),
    c(u = 0.6001, age = 30, male = 1, dsu = 0.5198960560),
    c(u = 0.5999, age = 70, male = 1, dsu = 0.4333173389),
    c(u = 0.6000, age = 70, male = 1, dsu = 0.4333885276),
    c(u = 0.6001, age = 70, male = 1, dsu = 0.5191445688)))

test_that("bwidth = \"default\" gives the DSU command's values, at 0.6 too", {
  f <- dsu_default_bw[["5L"]]
  got <- q(eqxwr_UK(f[, "u"], f[, "age"], f[, "male"], bwidth = "default"))
  expect_equal(got, unname(f[, "dsu"]), tolerance = 1e-6)
  f <- dsu_default_bw[["3L"]]
  got <- q(eqxw_UK(f[, "u"], f[, "age"], f[, "male"], bwidth = "default"))
  expect_equal(got, unname(f[, "dsu"]), tolerance = 1e-9)
})

# ── 3. no time-ordering message where there is no time variable ──────────────

messages_of <- function(expr) {
  m <- character(0)
  withCallingHandlers(suppressWarnings(expr), message = function(e) {
    m <<- c(m, conditionMessage(e)); invokeRestart("muffleMessage") })
  m
}

test_that("eq5d_profile_level_summary() says nothing about ordering time", {
  m <- messages_of(eq5d_profile_level_summary(
    example_data[example_data$time == "Pre-op", ], names_eq5d = DIMS,
    eq5d_version = "3L"))
  expect_false(any(grepl("ordering of time", m)))
})

test_that("where a time variable is given without its order, the message is spelt right", {
  m <- messages_of(eq5d_profile_dimension_change_table(
    example_data, name_id = "id", names_eq5d = DIMS, name_fu = "time"))
  expect_true(any(grepl("No ordering of time supplied", m, fixed = TRUE)))
  expect_false(any(grepl("suppled", m, fixed = TRUE)))
})

# ── 4. states outside the instrument are reported ────────────────────────────

warnings_of <- function(expr) {
  w <- character(0)
  val <- withCallingHandlers(suppressMessages(expr), warning = function(e) {
    w <<- c(w, conditionMessage(e)); invokeRestart("muffleWarning") })
  list(value = val, warnings = w)
}

test_that("EQ-5D-3L codes with a 4 or a 9 are NA with a warning; valid ones unchanged", {
  ref <- q(eq5d3l(c(11111, 12321), country = "GB"))
  r <- warnings_of(eq5d3l(c(11111, 41121, 12321, 91111), country = "GB"))
  expect_identical(r$value[c(1, 3)], ref)
  expect_true(all(is.na(r$value[c(2, 4)])))
  expect_length(r$warnings, 1L)
  expect_match(r$warnings, "2 health state", fixed = TRUE)
  expect_match(r$warnings, "EQ-5D-3L", fixed = TRUE)
})

test_that("dimension columns with levels outside the instrument are reported too", {
  df <- data.frame(mo = c(1, 4, 9, 2), sc = 1, ua = 1, pd = 1, ad = 1)
  r <- warnings_of(eq5d3l(df, country = "GB"))
  expect_true(all(is.na(r$value[2:3])))
  expect_false(anyNA(r$value[c(1, 4)]))
  expect_true(any(grepl("EQ-5D-3L", r$warnings, fixed = TRUE)))
})

test_that("every instrument and crosswalk reports its own range", {
  cases <- list(
    list(f = function(x) eq5d5l(x, country = "GB"),  bad = 61111, inst = "EQ-5D-5L"),
    list(f = function(x) eq5dy3l(x, country = "DE"), bad = 41111, inst = "EQ-5D-Y-3L"),
    list(f = function(x) eqxw(x, country = "GB"),    bad = 61111, inst = "EQ-5D-5L"),
    list(f = function(x) eqxwr(x, country = "GB"),   bad = 41111, inst = "EQ-5D-3L"))
  for (k in cases) {
    r <- warnings_of(k$f(c(11111, k$bad)))
    expect_false(is.na(r$value[1]), info = k$inst)
    expect_true(is.na(r$value[2]), info = k$inst)
    expect_true(any(grepl(k$inst, r$warnings, fixed = TRUE)), info = k$inst)
  }
})

test_that("with several value sets, each problem is reported once", {
  r <- warnings_of(eq5d3l(c(11111, 41121, 11111.5), country = c("GB", "US")))
  expect_length(r$warnings, 2L)   # one for the 4, one for the fraction
})

test_that("valid input gives no warning", {
  expect_no_warning(eq5d3l(make_all_EQ_indexes("3L"), country = "GB"))
  expect_no_warning(eq5d5l(make_all_EQ_indexes("5L"), country = "GB"))
})

# ── 5. toEQ5Ddims() returns integer columns ───────────────────────────────────

test_that("toEQ5Ddims() returns integer columns, like make_all_EQ_states()", {
  d <- toEQ5Ddims(make_all_EQ_indexes("3L"))
  s <- make_all_EQ_states("3L")
  expect_true(all(vapply(d, is.integer, NA)))
  expect_identical(d, s)
  # Codes it cannot read are still NA, now integer NA.
  d2 <- suppressWarnings(toEQ5Ddims(c(12321, 66666, NA)))
  expect_identical(unname(unlist(d2[1, ])), c(1L, 2L, 3L, 2L, 1L))
  expect_true(all(is.na(d2[2:3, ])))
  expect_true(all(vapply(d2, is.integer, NA)))
})

# ── 6-8. the help says what the functions do ─────────────────────────────────

help_text <- function(topic) {
  rd <- testthat::test_path("..", "..", "man", paste0(topic, ".Rd"))
  skip_if(!file.exists(rd), "package sources not available")
  out <- utils::capture.output(tools::Rd2txt(rd, options = list(underline_titles = FALSE)))
  gsub("\\s+", " ", gsub("['\"]", "", paste(out, collapse = " ")))
}

test_that("every value function documents missing and invalid responses", {
  for (topic in c("eq5d", "eq5d3l", "eq5d5l", "eq5dy3l", "eqxw", "eqxwr")) {
    h <- help_text(topic)
    expect_match(h, "Missing and invalid responses", fixed = TRUE, info = topic)
    expect_match(h, "gives NA without a warning", fixed = TRUE, info = topic)
    expect_match(h, "gives NA with a warning", fixed = TRUE, info = topic)
  }
})

test_that("what ?eq5d says about missing and invalid responses is what happens", {
  expect_no_warning(r <- q(eq5d3l(c(11111, NA), country = "GB")))
  expect_true(is.na(r[2]))
  expect_no_warning(eq5d3l(data.frame(mo = c(1, NA), sc = 1, ua = 1, pd = 1, ad = 1),
                           country = "GB"))
  for (bad in list(41111, 91111, 11111.5, 1111)) {
    r <- warnings_of(eq5d3l(c(11111, bad), country = "GB"))
    expect_true(is.na(r$value[2]), info = bad)
    expect_length(r$warnings, 1L)
  }
  ref <- q(eq5d3l(21111, country = "GB"))
  expect_identical(q(eq5d3l("21111", country = "GB")), ref)
  expect_identical(q(eq5d3l(data.frame(mo = factor(2), sc = 1, ua = 1, pd = 1, ad = 1),
                            country = "GB")), ref)
})

test_that("?eqvs_add describes its checks and its real return value", {
  h <- help_text("eqvs_add")
  expect_no_match(h, "True/False", fixed = TRUE)
  expect_match(h, "NULL, invisibly", fixed = TRUE)
  for (s in c("exactly two columns", "built-in value set code", "finite",
              "rows may be in any order", "returns 0"))
    expect_match(h, s, fixed = TRUE, info = s)
})

test_that("eqvs_add() behaves as its help says", {
  local_eq_env()
  df <- vs3("QDOC"); df <- df[rev(seq_len(nrow(df))), ]
  df$state <- as.character(df$state)            # any order, states as text
  r <- withVisible(q(eqvs_add(df, version = "3L", VSCode = "QDOC")))
  expect_null(r$value); expect_false(r$visible)
  expect_equal(q(eq5d3l(11111, country = "QDOC")), 1)
  # Own code again, in another case: a warning, 0, and the set is unchanged.
  df2 <- vs3("qdoc"); df2$qdoc <- 0
  expect_warning(r2 <- suppressMessages(eqvs_add(df2, version = "3L", VSCode = "qdoc")),
                 "already used")
  expect_identical(r2, 0)
  expect_equal(q(eq5d3l(11111, country = "QDOC")), 1)
  # A built-in code is an error.
  expect_error(q(eqvs_add(vs3("gb"), version = "3L", VSCode = "gb")), "built-in")
  # A missing value is an error.
  df3 <- vs3("QDNA"); df3$QDNA[3] <- NA
  expect_error(q(eqvs_add(df3, version = "3L", VSCode = "QDNA")))
})

test_that("?example_data gives the financial year and covers the value functions", {
  h <- help_text("example_data")
  expect_match(h, "Financial year", fixed = TRUE)
  expect_match(h, "value functions", fixed = TRUE)
  # ... and the value functions do warn about the 9s.
  d <- example_data[example_data$mo == 9, ][1, ]
  expect_warning(r <- suppressMessages(eq5d3l(d, country = "GB")), "EQ-5D-3L")
  expect_true(is.na(r))
})
