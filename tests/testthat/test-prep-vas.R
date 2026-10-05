# .prep_vas() prepares the EQ VAS column, which is recorded on a 0 to 100
# integer scale.
#
# It used to do `xorig <- x <- as.integer(x)`, which had two consequences:
#
#   * as.integer() truncates, so a VAS of 20.5 silently became 20 (H-6). The
#     function's own roxygen example passed 20.5, and a disabled block below it
#     showed an integer check had been intended;
#   * `xorig` held the already-truncated vector, so the comparison that raised
#     the warning could never detect the truncation, and the warning printed
#     the comparison itself rather than a count (H-5):
#
#       "TRUE observations were coerced to NAs as they were not interpretable
#        as integer values in the range allowed by the EQ-5D descriptive
#        system."
#
#     -- wrong count, and the wrong instrument: the column is the VAS, not the
#     descriptive system.

prep <- function(v) suppressWarnings(.prep_vas(data.frame(vas = v), "vas")$vas)

warnings_of <- function(expr) {
  w <- character(0)
  withCallingHandlers(expr,
    warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
  w
}

# ---------------------------------------------------------------------------
# H-5: the warning reports a count, and names the right instrument
# ---------------------------------------------------------------------------

test_that("the coercion warning counts the observations", {
  w <- warnings_of(.prep_vas(data.frame(vas = c(50, 150, -3, 999, NA)), "vas"))

  expect_length(w, 1L)
  expect_match(w, "^3 VAS observation\\(s\\) were coerced to NAs")
  # Not the comparison itself, which is what it used to print.
  expect_false(grepl("TRUE", w, fixed = TRUE))
  # The VAS, not the descriptive system.
  expect_match(w, "EQ VAS (0 to 100)", fixed = TRUE)
  expect_false(grepl("descriptive system", w, fixed = TRUE))
})

test_that("values already in range raise no warning", {
  expect_no_warning(.prep_vas(data.frame(vas = c(0L, 50L, 100L, NA)), "vas"))
})

test_that("input NAs are not counted as coercions", {
  # Four NAs in, one of them new.
  w <- warnings_of(.prep_vas(data.frame(vas = c(NA, NA, NA, 200)), "vas"))
  expect_match(w, "^1 VAS observation\\(s\\)")
})

# ---------------------------------------------------------------------------
# H-6: non-integers are rounded, and said so
# ---------------------------------------------------------------------------

test_that("non-integer values are rounded, not truncated", {
  # 20.5 was the value in the function's own example, and it became 20.
  expect_identical(prep(c(20.4, 20.6, 99.5, 0.5)), c(20L, 21L, 100L, 0L))
  # round() takes a half to the nearest even number, as everywhere else in R.
  expect_identical(prep(c(20.5, 21.5)), c(20L, 22L))
})

test_that("rounding is reported", {
  w <- warnings_of(.prep_vas(data.frame(vas = c(20.5, 30.25, 40, NA)), "vas"))

  expect_length(w, 1L)
  expect_match(w, "^2 VAS value\\(s\\) were not whole numbers")
  expect_match(w, "rounded to the nearest integer", fixed = TRUE)
})

test_that("rounding and coercion are reported separately", {
  w <- warnings_of(.prep_vas(data.frame(vas = c(20.5, 50, 150, -3)), "vas"))

  expect_length(w, 2L)
  expect_match(w[1], "^1 VAS value\\(s\\) were not whole numbers")
  expect_match(w[2], "^2 VAS observation\\(s\\) were coerced to NAs")
})

test_that("a whole number stored as a double is not reported as rounded", {
  expect_no_warning(.prep_vas(data.frame(vas = c(0, 50, 100)), "vas"))
  expect_identical(prep(c(0, 50, 100)), c(0L, 50L, 100L))
})

test_that("rounding happens before the range check", {
  # 99.6 rounds up to 100, which is in range; 100.6 rounds to 101, which is
  # not. Truncation used to keep both, at 99 and 100.
  expect_identical(prep(c(99.6, 100.6)), c(100L, NA_integer_))
})

# ---------------------------------------------------------------------------
# Types and edges
# ---------------------------------------------------------------------------

test_that("the result is an integer vector", {
  expect_type(prep(c(0, 50.5, 100)), "integer")
  expect_type(prep(c(NA, NA)), "integer")
})

test_that("the column is renamed to vas", {
  r <- suppressWarnings(.prep_vas(data.frame(score = c(50, 60)), "score"))
  expect_identical(names(r), "vas")
  expect_identical(r$vas, c(50L, 60L))
})

test_that("a character column is read as numbers", {
  expect_identical(prep(c("70", "80.6", "abc", NA)),
                   c(70L, 81L, NA_integer_, NA_integer_))
})

test_that("a factor column is read through its labels, not its codes", {
  # as.numeric() on a factor returns the level codes, so this used to give
  # 1 and 2 for a column holding 70 and 80.
  expect_identical(prep(factor(c("70", "80"))), c(70L, 80L))
})

test_that("a value too large for an integer is coerced without extra noise", {
  w <- warnings_of(.prep_vas(data.frame(vas = c(1e10, 5)), "vas"))
  expect_length(w, 1L)
  expect_match(w, "coerced to NAs")
  expect_identical(prep(c(1e10, 5)), c(NA_integer_, 5L))
})

test_that("an all-NA column is left alone", {
  expect_no_warning(.prep_vas(data.frame(vas = c(NA_real_, NA_real_)), "vas"))
  expect_identical(prep(c(NA_real_, NA_real_)), c(NA_integer_, NA_integer_))
})

test_that("a zero-row column is handled", {
  expect_no_warning(.prep_vas(data.frame(vas = numeric(0)), "vas"))
  expect_identical(prep(numeric(0)), integer(0))
})

# ---------------------------------------------------------------------------
# Through the exported functions
# ---------------------------------------------------------------------------

test_that("eq5d_vas_summary() reports a usable count for example_data", {
  # 999 is the missing-VAS code, so those observations are coerced.
  expected <- sum(!example_data$vas %in% 0:100)
  w <- warnings_of(suppressMessages(
    eq5d_vas_summary(example_data, name_vas = "vas", name_fu = "time",
                     levels_fu = c("Pre-op", "Post-op"))))

  expect_length(w, 1L)
  expect_match(w, paste0("^", expected, " VAS observation\\(s\\)"))
})

test_that("the VAS summary values are unchanged", {
  r <- suppressWarnings(suppressMessages(
    eq5d_vas_summary(example_data, name_vas = "vas", name_fu = "time",
                     levels_fu = c("Pre-op", "Post-op"))))

  # example_data holds whole numbers only, so nothing is rounded and the
  # summary is exactly what it was before the fix.
  expect_equal(sum(example_data$vas != round(example_data$vas)), 0)
  expect_equal(r[["Pre-op"]][r$name == "Mean"],  70.02878, tolerance = 1e-5)
  expect_equal(r[["Post-op"]][r$name == "Mean"], 76.13063, tolerance = 1e-5)
  expect_equal(r[["Pre-op"]][r$name == "Observations"],  4552)
  expect_equal(r[["Post-op"]][r$name == "Missing (n)"],  223)
})

test_that("every VAS function rounds and warns the same way", {
  # Column named `fu` so the default name_fu applies to all three.
  df <- data.frame(vas = c(20.5, 50, 150),
                   fu  = c("Pre-op", "Post-op", "Pre-op"))
  calls <- list(
    eq5d_vas_summary            = function() eq5d_vas_summary(df, name_vas = "vas"),
    eq5d_vas_distribution_table = function() eq5d_vas_distribution_table(df, name_vas = "vas"),
    eq5d_vas_histogram          = function() eq5d_vas_histogram(df, name_vas = "vas"))

  for (nm in names(calls)) {
    w <- warnings_of(suppressMessages(calls[[nm]]()))
    expect_true(any(grepl("were not whole numbers", w)), info = nm)
    expect_true(any(grepl("coerced to NAs", w)), info = nm)
    expect_false(any(grepl("TRUE observations", w)), info = nm)
  }
})
