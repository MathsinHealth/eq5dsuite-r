# eq5d_vas_summary() and eq5d_utility_summary() lay their follow-up levels out
# as columns. Both go through .summary_cts_by_fu(), which built the rows in the
# requested order and then ran them through merge(..., by = "fu"). merge()
# sorts by the `by` column, so the order was discarded and the columns always
# came out alphabetically:
#
#   levels_fu = c("Pre-op", "Post-op")  ->  name | Post-op | Pre-op
#   levels_fu = c("Post-op", "Pre-op")  ->  name | Post-op | Pre-op
#
# The values were always correct; only their position was wrong. Nothing in the
# output labels which column is baseline, so a reader could easily take the
# first one as the starting point (Post-op mean 76.1 against Pre-op 70.0 in
# example_data).

dims <- c("mo", "sc", "ua", "pd", "ad")

vas_summary <- function(df, levels_fu, ...) {
  suppressWarnings(suppressMessages(
    eq5d_vas_summary(df, name_vas = "vas", name_fu = "time",
                     levels_fu = levels_fu, ...)))
}

# eq5d_utility_summary() analyses a value column, so give the frame one.
valued <- function(df) {
  df$value <- suppressWarnings(suppressMessages(
    eq5d3l(df[, dims], country = "GB")))
  df
}

utility_summary <- function(df, levels_fu, ...) {
  suppressWarnings(suppressMessages(
    eq5d_utility_summary(valued(df), name_utility = "value", name_fu = "time",
                         levels_fu = levels_fu, ...)))
}

# ---------------------------------------------------------------------------
# The requested order is honoured
# ---------------------------------------------------------------------------

test_that("eq5d_vas_summary() returns columns in the requested order", {
  expect_identical(names(vas_summary(example_data, c("Pre-op", "Post-op"))),
                   c("name", "Pre-op", "Post-op"))
  # Not merely "not alphabetical": reversing the request reverses the columns.
  expect_identical(names(vas_summary(example_data, c("Post-op", "Pre-op"))),
                   c("name", "Post-op", "Pre-op"))
})

test_that("eq5d_utility_summary() returns columns in the requested order", {
  expect_identical(names(utility_summary(example_data, c("Pre-op", "Post-op"))),
                   c("name", "Pre-op", "Post-op"))
  expect_identical(names(utility_summary(example_data, c("Post-op", "Pre-op"))),
                   c("name", "Post-op", "Pre-op"))
})

test_that("more than two follow-up levels keep their order", {
  df <- example_data
  df$time <- rep(c("Baseline", "Month 3", "Month 12", "Month 24"),
                 length.out = nrow(df))
  lv <- c("Baseline", "Month 3", "Month 12", "Month 24")   # not alphabetical

  expect_identical(names(vas_summary(df, lv)), c("name", lv))
  expect_identical(names(vas_summary(df, rev(lv))), c("name", rev(lv)))
})

# ---------------------------------------------------------------------------
# Only the position moved
# ---------------------------------------------------------------------------

test_that("each column still holds its own follow-up level's values", {
  a <- vas_summary(example_data, c("Pre-op", "Post-op"))
  b <- vas_summary(example_data, c("Post-op", "Pre-op"))

  # Same table, columns swapped -- not the values shifting with them.
  expect_identical(a$name, b$name)
  expect_identical(a[["Pre-op"]],  b[["Pre-op"]])
  expect_identical(a[["Post-op"]], b[["Post-op"]])

  # And the values are the ones the data actually holds. example_data codes a
  # missing VAS as 999, which .prep_vas() turns into NA, so the usable values
  # are those inside the 0-100 range.
  usable <- function(fu) {
    v <- example_data$vas[example_data$time == fu]
    v[v %in% 0:100]
  }
  pre  <- usable("Pre-op")
  post <- usable("Post-op")
  expect_equal(a[["Pre-op"]][a$name == "Mean"],  mean(pre))
  expect_equal(a[["Post-op"]][a$name == "Mean"], mean(post))
  expect_equal(a[["Pre-op"]][a$name == "Observations"],  length(pre))
  expect_equal(a[["Post-op"]][a$name == "Observations"], length(post))
  expect_equal(a[["Pre-op"]][a$name == "Total sample"],
               sum(example_data$time == "Pre-op"))
})

test_that("the statistic rows are unchanged", {
  r <- vas_summary(example_data, c("Pre-op", "Post-op"))
  expect_identical(
    r$name,
    c("Mean", "Standard error", "Median", "Mode", "Standard deviation",
      "Kurtosis (non-excess)", "Skewness", "Minimum", "Maximum", "Range",
      "Observations",
      "Missing (n)", "Total sample", "Missing (%)"))
})

# ---------------------------------------------------------------------------
# Edges
# ---------------------------------------------------------------------------

test_that("a follow-up level with no observations keeps its position", {
  df <- example_data
  df$time <- as.character(df$time)
  r <- vas_summary(df, c("Pre-op", "Interim", "Post-op"))

  expect_identical(names(r), c("name", "Pre-op", "Interim", "Post-op"))
  expect_equal(r[["Interim"]][r$name == "Observations"], 0)
  # The levels around it are untouched.
  pre <- example_data$vas[example_data$time == "Pre-op"]
  expect_equal(r[["Pre-op"]][r$name == "Observations"], sum(pre %in% 0:100))
})

test_that("a single follow-up level still works", {
  df <- example_data[example_data$time == "Pre-op", ]
  df$time <- as.character(df$time)
  r <- vas_summary(df, "Pre-op")
  expect_identical(names(r), c("name", "Pre-op"))
  expect_equal(r[["Pre-op"]][r$name == "Total sample"], nrow(df))
})

test_that(".summary_cts_by_fu() honours the factor's levels directly", {
  df <- data.frame(
    fu = factor(rep(c("Post-op", "Pre-op"), each = 3),
                levels = c("Pre-op", "Post-op")),
    v  = c(4, 5, 6, 1, 2, 3))
  r <- .summary_cts_by_fu(df, name_v = "v")

  expect_identical(names(r), c("name", "Pre-op", "Post-op"))
  expect_equal(r[["Pre-op"]][r$name == "Mean"],  2)
  expect_equal(r[["Post-op"]][r$name == "Mean"], 5)
})

# ---------------------------------------------------------------------------
# The other levels_fu functions were not affected
# ---------------------------------------------------------------------------

test_that("functions that lay follow-up out as columns elsewhere are correct", {
  # .freqtab() re-sorts explicitly after its own merge(), so this one was
  # always right; it is here so that a regression in either helper is caught.
  # (eq5d_profile_change_summary() forwards match.call() to do.call(), so its
  # arguments have to be literals, not variables from this frame.)
  r <- suppressWarnings(suppressMessages(
    eq5d_profile_change_summary(example_data,
                                names_eq5d = c("mo", "sc", "ua", "pd", "ad"),
                                name_fu = "time",
                                levels_fu = c("Pre-op", "Post-op"),
                                eq5d_version = "3L")))
  mo <- grep("_mo$", names(r), value = TRUE)
  expect_identical(mo, c("n_Pre-op_mo", "freq_Pre-op_mo",
                         "n_Post-op_mo", "freq_Post-op_mo"))
})

test_that("plot functions carry the order in the factor levels", {
  # Row order in plot_data is cosmetic; what orders the axes is the factor.
  r <- suppressWarnings(suppressMessages(
    eq5d_utility_over_time_plot(valued(example_data), name_utility = "value",
                                name_fu = "time",
                                levels_fu = c("Pre-op", "Post-op"))))
  expect_identical(levels(r$plot_data$fu), c("Pre-op", "Post-op"))
})
