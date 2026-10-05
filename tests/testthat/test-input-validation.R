# Four review findings about what the analysis functions do with input they
# cannot use. All of them produced either a cryptic internal error or a
# silently wrong result.

dims <- c("mo", "sc", "ua", "pd", "ad")

quiet_err <- function(expr)
  tryCatch(suppressWarnings(suppressMessages(expr)), error = conditionMessage)

# ---------------------------------------------------------------------------
# M-8. The missing-column error names the column and the argument
# ---------------------------------------------------------------------------

test_that("the missing-column error names the column and where it came from", {
  # The review's own evidence: a frame with exactly the five EQ-5D columns
  # fails because name_fu defaulted to "fu", which the caller never chose.
  d <- data.frame(mo = 1L, sc = 1L, ua = 1L, pd = 1L, ad = 1L, utility = 1)
  msg <- quiet_err(eq5d_utility_summary(d, name_utility = "utility"))

  expect_match(msg, "not found in the data frame", fixed = TRUE)
  expect_match(msg, "\"fu\" (from `name_fu`)", fixed = TRUE)
  expect_match(msg, "filled with defaults", fixed = TRUE)
  # And it shows what the frame does have.
  expect_match(msg, "\"mo\", \"sc\", \"ua\", \"pd\", \"ad\"", fixed = TRUE)
  # The same error names a missing value column against its own argument.
  msg2 <- quiet_err(eq5d_utility_summary(
    data.frame(fu = "a", stringsAsFactors = FALSE), name_utility = "value"))
  expect_match(msg2, "\"value\" (from `name_utility`)", fixed = TRUE)
})

test_that("several missing columns are grouped by their argument", {
  d <- data.frame(mo = 1L, sc = 1L, ua = 1L, pd = 1L, ad = 1L)
  msg <- quiet_err(eq5d_profile_pchc_table(
    d, name_id = "patient", names_eq5d = dims, name_fu = "visit",
    levels_fu = c("a", "b")))

  expect_match(msg, "\"patient\" (from `name_id`)", fixed = TRUE)
  expect_match(msg, "\"visit\" (from `name_fu`)", fixed = TRUE)
})

test_that("misspelled EQ-5D columns are listed individually", {
  d <- data.frame(mo = 1L, sc = 1L, ua = 1L, pd = 1L, ad = 1L)
  msg <- quiet_err(eq5d_profile_top_states(
    d, names_eq5d = c("MO", "sc", "ua", "pd", "typo"),
    eq5d_version = "3L", n = 3))

  expect_match(msg, "\"MO\", \"typo\" (from `names_eq5d`)", fixed = TRUE)
})

test_that("every analysis function uses the shared check", {
  # Guarded on the file, not the directory: under covr the package is built
  # elsewhere and R/ exists without the sources in it.
  f <- testthat::test_path("..", "..", "R", "eq5d_devlin.R")
  skip_if(!file.exists(f), "package sources not available")
  txt <- readLines(f, warn = FALSE)

  expect_identical(grep("Provided column names", txt, value = TRUE),
                   character(0))
  expect_gte(length(grep(".check_columns(df", txt, fixed = TRUE)), 26L)
})

test_that(".check_columns() skips arguments that are NULL or empty", {
  d <- data.frame(a = 1, b = 2)
  expect_true(.check_columns(d, x = NULL, y = character(0), z = "a"))
  expect_error(.check_columns(d, z = "c"), "\"c\" (from `z`)", fixed = TRUE)
})

test_that("a wide data frame does not print every column name", {
  d <- as.data.frame(setNames(as.list(seq_len(40)), paste0("v", 1:40)))
  msg <- quiet_err(.check_columns(d, name_fu = "fu"))
  expect_match(msg, "and 25 more", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# M-9. Every observation invalid
# ---------------------------------------------------------------------------

test_that("all-invalid data gives an error that says so", {
  d <- data.frame(mo = c(9L, 9L), sc = c(9L, 9L), ua = c(9L, 9L),
                  pd = c(9L, 9L), ad = c(9L, 9L),
                  time = c("Pre-op", "Post-op"), vas = c(50L, 60L),
                  id = c(1L, 1L), value = c(0.5, 0.5))

  # Seven of these used to surface aggregate()'s "no rows to aggregate", and
  # one "incorrect number of dimensions".
  calls <- list(
    function() eq5d_profile_level_summary(d, names_eq5d = dims, eq5d_version = "3L"),
    function() eq5d_profile_top_states(d, names_eq5d = dims, eq5d_version = "3L", n = 3),
    function() eq5d_profile_lfs_distribution(d, names_eq5d = dims, eq5d_version = "3L"),
    function() eq5d_profile_density_curve(d, names_eq5d = dims, eq5d_version = "3L"),
    function() eq5d_profile_lss_utility_plot(d, names_eq5d = dims,
                                             name_utility = "value",
                                             eq5d_version = "3L"),
    function() eq5d_profile_pchc_table(d, name_id = "id", names_eq5d = dims,
                                       name_fu = "time",
                                       levels_fu = c("Pre-op", "Post-op")))

  for (f in calls) {
    msg <- quiet_err(f())
    expect_match(msg, "None of the 2 observations hold a usable EQ-5D health state",
                 fixed = TRUE)
    expect_false(grepl("no rows to aggregate", msg, fixed = TRUE))
    expect_false(grepl("incorrect number of dimensions", msg, fixed = TRUE))
  }
})

test_that("the error points at the version and the missing-value coding", {
  d <- data.frame(mo = 4L, sc = 4L, ua = 4L, pd = 4L, ad = 4L)
  msg <- quiet_err(eq5d_profile_level_summary(d, names_eq5d = dims,
                                              eq5d_version = "3L"))
  # A 5L dataset read as 3L is a plausible cause, so the instrument is named.
  expect_match(msg, "EQ-5D-3L", fixed = TRUE)
  expect_match(msg, "outside 1 to 3", fixed = TRUE)

  msg5 <- quiet_err(eq5d_profile_level_summary(
    data.frame(mo = 9L, sc = 9L, ua = 9L, pd = 9L, ad = 9L),
    names_eq5d = dims, eq5d_version = "5L"))
  expect_match(msg5, "EQ-5D-5L", fixed = TRUE)
  expect_match(msg5, "outside 1 to 5", fixed = TRUE)
})

test_that("one usable observation is enough to proceed", {
  d <- data.frame(mo = c(1L, 9L), sc = c(1L, 9L), ua = c(1L, 9L),
                  pd = c(1L, 9L), ad = c(1L, 9L))
  r <- suppressWarnings(suppressMessages(
    eq5d_profile_level_summary(d, names_eq5d = dims, eq5d_version = "3L")))
  expect_s3_class(r, "data.frame")
  expect_gt(nrow(r), 0L)
})

test_that("a zero-row data frame is not treated as all-invalid", {
  d <- data.frame(mo = integer(0), sc = integer(0), ua = integer(0),
                  pd = integer(0), ad = integer(0))
  expect_no_error(.prep_eq5d(d, names = dims, eq5d_version = "3L"))
})

# ---------------------------------------------------------------------------
# M-16. Empty follow-up levels
# ---------------------------------------------------------------------------

test_that("a follow-up level with no data gives NA, not NaN or Inf", {
  df <- example_data
  df$time <- as.character(df$time)
  r <- suppressWarnings(suppressMessages(
    eq5d_vas_summary(df, name_vas = "vas", name_fu = "time",
                     levels_fu = c("Pre-op", "Interim", "Post-op"))))

  v <- r[["Interim"]]
  expect_false(any(is.nan(v)))
  expect_false(any(is.infinite(v)))
  # Counts are genuinely zero; every statistic is NA.
  expect_identical(v[r$name == "Observations"], 0)
  expect_identical(v[r$name == "Total sample"], 0)
  for (stat in c("Mean", "Median", "Minimum", "Maximum", "Range",
                 "Standard deviation", "Kurtosis (non-excess)", "Skewness",
                 "Missing (%)"))
    expect_true(is.na(v[r$name == stat]), info = stat)
})

test_that("the empty levels are named in a warning", {
  df <- example_data
  df$time <- as.character(df$time)
  w <- character(0)
  withCallingHandlers(
    suppressMessages(eq5d_vas_summary(df, name_vas = "vas", name_fu = "time",
                                      levels_fu = c("Pre-op", "Interim",
                                                    "Late", "Post-op"))),
    warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })

  hit <- grep("follow-up level", w, value = TRUE)
  expect_length(hit, 1L)
  expect_match(hit, "\"Interim\", \"Late\"", fixed = TRUE)
  expect_match(hit, "are NA", fixed = TRUE)
  # One warning for all of them, not one each.
  expect_match(hit, "levels", fixed = TRUE)
})

test_that("levels that do have data are unaffected", {
  df <- example_data
  df$time <- as.character(df$time)
  with_gap <- suppressWarnings(suppressMessages(
    eq5d_vas_summary(df, name_vas = "vas", name_fu = "time",
                     levels_fu = c("Pre-op", "Interim", "Post-op"))))
  without <- suppressWarnings(suppressMessages(
    eq5d_vas_summary(df, name_vas = "vas", name_fu = "time",
                     levels_fu = c("Pre-op", "Post-op"))))

  expect_identical(with_gap[["Pre-op"]],  without[["Pre-op"]])
  expect_identical(with_gap[["Post-op"]], without[["Post-op"]])
})

test_that("no warning when every level has data", {
  w <- character(0)
  withCallingHandlers(
    suppressMessages(eq5d_vas_summary(example_data, name_vas = "vas",
                                      name_fu = "time",
                                      levels_fu = c("Pre-op", "Post-op"))),
    warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
  expect_length(grep("follow-up level", w), 0L)
})

# ---------------------------------------------------------------------------
# M-17. Columns already carrying a standard dimension name
# ---------------------------------------------------------------------------

test_that("a non-EQ-5D column named mo is reported, not silently overwritten", {
  # A follow-up column literally called "mo". This used to fail deep inside
  # with "arguments imply differing number of rows: 2, 1, 0".
  d <- data.frame(q1 = c(1L, 3L), q2 = c(1L, 3L), q3 = c(1L, 3L),
                  q4 = c(1L, 3L), q5 = c(1L, 3L), id = c(1L, 1L),
                  mo = c("Pre-op", "Post-op"), stringsAsFactors = FALSE)
  msg <- quiet_err(eq5d_profile_pchc_table(
    d, name_id = "id", names_eq5d = c("q1", "q2", "q3", "q4", "q5"),
    name_fu = "mo", levels_fu = c("Pre-op", "Post-op")))

  expect_match(msg, "a column named \"mo\"", fixed = TRUE)
  expect_match(msg, "would be overwritten", fixed = TRUE)
  expect_match(msg, "rename it", fixed = TRUE)
  expect_false(grepl("differing number of rows", msg, fixed = TRUE))
})

test_that("several colliding columns are named together", {
  d <- data.frame(q1 = 1L, q2 = 1L, q3 = 1L, q4 = 1L, q5 = 1L,
                  mo = "x", sc = "y", stringsAsFactors = FALSE)
  msg <- quiet_err(.prep_eq5d(d, names = c("q1", "q2", "q3", "q4", "q5"),
                              eq5d_version = "3L"))
  expect_match(msg, "columns named \"mo\", \"sc\"", fixed = TRUE)
})

test_that("reordering or renaming among the five is still fine", {
  # The rename is positional, so the caller's "sc" column can be mobility.
  d <- data.frame(sc = c(1L, 3L), mo = c(2L, 3L), ua = 1L, pd = 1L, ad = 1L)
  r <- .prep_eq5d(d, names = c("sc", "mo", "ua", "pd", "ad"),
                  eq5d_version = "3L", add_state = TRUE)

  expect_identical(names(r)[1:5], dims)
  expect_identical(r$mo, c(1L, 3L))   # from the caller's `sc` column
  expect_identical(r$sc, c(2L, 3L))   # from the caller's `mo` column
  expect_identical(r$state, c("12111", "33111"))
})

test_that("the ordinary case is untouched", {
  d <- data.frame(mo = c(1L, 3L), sc = c(1L, 3L), ua = c(1L, 3L),
                  pd = c(1L, 3L), ad = c(1L, 3L))
  expect_no_error(.prep_eq5d(d, names = dims, eq5d_version = "3L"))

  # And extra columns that are not dimension names are fine.
  d$time <- c("Pre-op", "Post-op")
  d$vas <- c(50L, 60L)
  expect_no_error(.prep_eq5d(d, names = dims, eq5d_version = "3L"))
})

# ---------------------------------------------------------------------------
# S-2. A missing value in the grouping variable
#
# eq5d_utility_summary_by_group() turns each group into a column of the
# returned table. A group value of NA cannot name a column, and the pivot at
# the end of the function failed with "missing value where TRUE/FALSE needed"
# -- on example_data, whose `gender` column is NA for 970 of its 10,000 rows.
#
# The four sibling by-group functions keep the missing group as a group of its
# own, and eq5d_profile_level_summary_by_group(), which also turns group values
# into column names, labels it "NA". This one now does the same.
# ---------------------------------------------------------------------------

# example_data with the GB values the by-group tests below summarise.
valued_example <- function() {
  d <- example_data
  d$utility <- suppressWarnings(suppressMessages(
    .prep_eq5d(d[, dims], names = dims, add_state = TRUE, add_utility = TRUE,
               eq5d_version = "3L", country = "GB")$utility))
  d
}

test_that("a missing group value does not break the summary", {
  # The premise: example_data really does have missing genders.
  expect_gt(sum(is.na(example_data$gender)), 0L)

  r <- suppressWarnings(suppressMessages(
    eq5d_utility_summary_by_group(valued_example(), name_utility = "utility",
                                  name_groupvar = "gender")))

  expect_s3_class(r, "data.frame")
  expect_identical(names(r),
                   c("name", "Female", "Male", "NA", "Not specified",
                     "All groups"))
  expect_identical(r$name,
                   c("Mean", "Standard error", "Median", "25th", "75th", "N",
                     "Missing"))
})

test_that("the missing group holds the rows whose group is missing", {
  r <- suppressWarnings(suppressMessages(
    eq5d_utility_summary_by_group(valued_example(), name_utility = "utility",
                                  name_groupvar = "gender")))

  # Computed independently of the function under test.
  d <- valued_example()
  u <- d$utility[is.na(d$gender)]

  expect_equal(r[["NA"]][r$name == "Mean"], mean(u, na.rm = TRUE))
  expect_equal(r[["NA"]][r$name == "N"], sum(!is.na(u)))
  expect_equal(r[["NA"]][r$name == "Missing"], sum(is.na(u)))

  # The totals cover every row, missing group included.
  expect_equal(r[["All groups"]][r$name == "N"] +
                 r[["All groups"]][r$name == "Missing"],
               nrow(example_data))
})

test_that("it agrees with the sibling that plots the same groups", {
  # eq5d_utility_by_group_plot() keeps the missing group too; the group means
  # must match.
  tbl <- suppressWarnings(suppressMessages(
    eq5d_utility_summary_by_group(valued_example(), name_utility = "utility",
                                  name_groupvar = "gender")))
  plt <- suppressWarnings(suppressMessages(
    eq5d_utility_by_group_plot(valued_example(), name_utility = "utility",
                               name_groupvar = "gender")))$plot_data

  key <- as.character(plt$groupvar)
  key[is.na(key)] <- "NA"
  for (g in c("Female", "Male", "Not specified", "NA", "All groups"))
    expect_equal(tbl[[g]][tbl$name == "Mean"], plt$mean[key == g], info = g)
})

test_that("data with no missing group is unaffected", {
  d <- valued_example()
  d <- d[!is.na(d$gender), ]
  r <- suppressWarnings(suppressMessages(
    eq5d_utility_summary_by_group(d, name_utility = "utility",
                                  name_groupvar = "gender")))

  expect_identical(names(r),
                   c("name", "Female", "Male", "Not specified", "All groups"))
  expect_false("NA" %in% names(r))
})

test_that("a factor grouping variable gives the same answer as a character", {
  # The pivot used to index by a factor's integer code rather than its label.
  d <- valued_example()
  d <- d[!is.na(d$gender), ]
  chr <- suppressWarnings(suppressMessages(
    eq5d_utility_summary_by_group(d, name_utility = "utility",
                                  name_groupvar = "gender")))
  d$gender <- factor(d$gender, levels = c("Male", "Female", "Not specified"))
  fct <- suppressWarnings(suppressMessages(
    eq5d_utility_summary_by_group(d, name_utility = "utility",
                                  name_groupvar = "gender")))

  expect_setequal(names(fct), names(chr))
  expect_equal(fct[, sort(names(fct))], chr[, sort(names(chr))])
})
