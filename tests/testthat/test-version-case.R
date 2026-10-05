# Version arguments are documented as accepting either case. Several functions
# validated them with toupper() but then branched on an upper-case literal, so
# a lower-case version selected the wrong instrument:
#
#   * .prep_eq5d(eq5d_version = "3l") accepted levels 4 and 5 for a
#     three-level instrument, and they reached user-facing tables (H-2);
#   * .get_lfs("12345", "5l") returned a three-digit Level Frequency Score,
#     silently dropping the level-4 and level-5 counts (H-4);
#   * make_all_EQ_states("5l") returned the 243 three-level states;
#   * make_dummies(version = "5l") built three-level dummies.
#
# .norm_version() is now the single place a version argument is checked.

# ---------------------------------------------------------------------------
# The normaliser
# ---------------------------------------------------------------------------

test_that(".norm_version() canonicalises every accepted spelling", {
  for (v in c("3L", "3l", " 3l ")) expect_identical(.norm_version(v), "3L")
  for (v in c("5L", "5l"))         expect_identical(.norm_version(v), "5L")
  for (v in c("Y3L", "y3l", "Y3l")) expect_identical(.norm_version(v), "Y3L")
})

test_that(".norm_version() rejects anything else", {
  expect_error(.norm_version("4L"), "Unknown EQ-5D version '4L'")
  # The message quotes what the caller actually wrote, not the uppercased form.
  expect_error(.norm_version("xw"), "Unknown EQ-5D version 'xw'")
  expect_error(.norm_version(NULL), "must be a single string")
  expect_error(.norm_version(NA_character_), "must be a single string")
  expect_error(.norm_version(c("3L", "5L")), "must be a single string")
  expect_error(.norm_version(5), "must be a single string")
  # `allowed` narrows the set, and `arg` names the argument.
  expect_error(.norm_version("Y3L", allowed = c("3L", "5L")),
               "Unknown EQ-5D version 'Y3L'")
  expect_error(.norm_version(NULL, arg = "eq5d_version"),
               "Argument 'eq5d_version'", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# H-2: level-range validation
# ---------------------------------------------------------------------------

test_that(".prep_eq5d() applies the three-level range whatever the case", {
  dims <- c("mo", "sc", "ua", "pd", "ad")
  df <- data.frame(mo = c(1L, 5L), sc = 1L, ua = 1L, pd = 1L, ad = 1L)

  for (v in c("3L", "3l", "Y3L", "y3l")) {
    expect_warning(.prep_eq5d(df, names = dims, eq5d_version = v),
                   "not a level the instrument allows")
    got <- suppressWarnings(
      .prep_eq5d(df, names = dims, eq5d_version = v, add_state = TRUE))
    expect_identical(got$mo, c(1L, NA_integer_), info = v)
    expect_identical(got$state, c("11111", NA_character_), info = v)
  }

  for (v in c("5L", "5l")) {
    got <- .prep_eq5d(df, names = dims, eq5d_version = v, add_state = TRUE)
    expect_identical(got$mo, c(1L, 5L), info = v)
    expect_identical(got$state, c("11111", "51111"), info = v)
  }
})

test_that(".prep_eq5d() rejects an unknown version before coercing anything", {
  dims <- c("mo", "sc", "ua", "pd", "ad")
  df <- data.frame(mo = c(1L, 5L), sc = 1L, ua = 1L, pd = 1L, ad = 1L)

  # The check used to run after the range check, so a bad version produced a
  # coercion warning on the way to the error.
  expect_error(.prep_eq5d(df, names = dims, eq5d_version = "XWR"),
               "Unknown EQ-5D version 'XWR'")
  expect_no_warning(
    try(.prep_eq5d(df, names = dims, eq5d_version = "XWR"), silent = TRUE))
})

test_that(".prep_eq5d() keeps the wider range when no version is given", {
  dims <- c("mo", "sc", "ua", "pd", "ad")
  df <- data.frame(mo = c(1L, 5L), sc = 1L, ua = 1L, pd = 1L, ad = 1L)
  got <- .prep_eq5d(df, names = dims, eq5d_version = NULL)
  expect_identical(got$mo, c(1L, 5L))
})

test_that("lower-case versions no longer leak levels into level summaries", {
  df <- data.frame(mo = c(1L, 2L, 4L, 5L), sc = 1L, ua = 1L, pd = 1L, ad = 1L)

  for (v in c("3L", "3l")) {
    r <- suppressWarnings(suppressMessages(
      eq5d_profile_level_summary(df, names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
                                 eq5d_version = v)))
    expect_false(any(r$level %in% c("4", "5")), info = v)
    expect_true(any(grepl("levels 2+3)", r$level, fixed = TRUE)), info = v)
    expect_false(any(grepl("2+3+4+5", r$level, fixed = TRUE)), info = v)
  }
})

# ---------------------------------------------------------------------------
# H-4: Level Frequency Score
# ---------------------------------------------------------------------------

test_that(".get_lfs() counts levels 4 and 5 whatever the case", {
  for (v in c("5L", "5l")) expect_identical(.get_lfs("12345", v), "11111")
  for (v in c("3L", "3l", "Y3L", "y3l"))
    expect_identical(.get_lfs("12345", v), "111")

  expect_identical(.get_lfs(c("11111", NA), "5l"), c("50000", NA_character_))
  expect_error(.get_lfs("12345", "4L"), "Unknown EQ-5D version")
})

test_that("lower-case versions give the same LFS distribution as upper-case", {
  df <- data.frame(mo = c(1L, 2L, 5L), sc = c(1L, 3L, 5L), ua = 1L,
                   pd = 1L, ad = c(1L, 4L, 5L))
  args <- list(df = df, names_eq5d = c("mo", "sc", "ua", "pd", "ad"))

  upper <- suppressMessages(do.call(eq5d_profile_lfs_distribution,
                                    c(args, eq5d_version = "5L")))
  lower <- suppressMessages(do.call(eq5d_profile_lfs_distribution,
                                    c(args, eq5d_version = "5l")))
  expect_identical(upper, lower)
  expect_true(all(nchar(upper$lfs[!is.na(upper$lfs)]) == 5))
})

# ---------------------------------------------------------------------------
# The same defect elsewhere
# ---------------------------------------------------------------------------

test_that("make_all_EQ_states() and make_all_EQ_indexes() honour the case", {
  expect_identical(make_all_EQ_states("5l"), make_all_EQ_states("5L"))
  expect_identical(nrow(make_all_EQ_states("5l")), 3125L)
  expect_identical(nrow(make_all_EQ_states("3l")), 243L)
  expect_identical(make_all_EQ_indexes("5l"), make_all_EQ_indexes("5L"))
  # The EQ-5D-Y-3L has the 3L's 243 states (accepted since F06, for the
  # Health Profile Grid); a version that does not exist is still refused.
  expect_identical(make_all_EQ_states("y3l"), make_all_EQ_states("3L"))
  expect_identical(make_all_EQ_indexes("Y3L"), make_all_EQ_indexes("3L"))
  expect_error(make_all_EQ_states("4L"), "Unknown EQ-5D version '4L'")
})

test_that("make_dummies() honours the case and rejects an unknown version", {
  df <- data.frame(mo = c(5L, 1L), sc = c(4L, 1L), ua = c(3L, 1L),
                   pd = c(2L, 1L), ad = c(1L, 1L))
  d5 <- make_dummies(df, version = "5L")
  expect_identical(make_dummies(df, version = "5l"), d5)
  expect_false(anyNA(d5))
  # Before the fix "5l" fell through to the three-level design matrix and
  # levels 4 and 5 indexed past its end.
  expect_identical(ncol(d5), 20L)

  expect_error(make_dummies(df, version = "4L"), "Unknown EQ-5D version")
})

test_that("eqvs_add() and eqvs_drop() accept a lower-case version", {
  cache_dir <- withr::local_tempdir()
  local_eq_env(cache_dir)

  expect_message(
    eqvs_add(dummy_vs("3L", "FAN"), version = "3l", country = "Fantasia",
             countryCode = "FA", VSCode = "FAN", saveOption = 1),
    "added")
  expect_true("FAN" %in% eqvs_display(version = "3l",
                                      return_df = TRUE)$VS_code)

  expect_message(
    eqvs_drop(country = "FAN", version = "3l", saveOption = 1, ask = FALSE),
    "removed|dropped|deleted")
  expect_false("FAN" %in% eqvs_display(version = "3L",
                                       return_df = TRUE)$VS_code)
})

test_that("eq5d() and .fixCountries() were already case-insensitive", {
  local_eq_env()
  expect_identical(unname(eq5d(11111, country = "GB", version = "5l")),
                   unname(eq5d(11111, country = "GB", version = "5L")))
  expect_identical(.fixCountries("GB", "5l"), .fixCountries("GB", "5L"))
})
