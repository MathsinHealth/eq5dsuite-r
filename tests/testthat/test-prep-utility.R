# The eq5d_utility_* functions analyse a column of pre-calculated EQ-5D values
# instead of calculating values from the dimensions. .prep_utility() is the
# step that reads that column.

dims <- c("mo", "sc", "ua", "pd", "ad")
q <- function(expr) suppressWarnings(suppressMessages(expr))

test_that("a numeric column passes through and is renamed", {
  d <- data.frame(value = c(1, 0.5, -0.594, NA))
  r <- .prep_utility(d, "value")
  expect_identical(names(r), "utility")
  expect_equal(r$utility, c(1, 0.5, -0.594, NA))
})

test_that("negative values are kept", {
  # EQ-5D values are negative for states regarded as worse than dead, so there
  # is no lower bound to check against.
  r <- .prep_utility(data.frame(u = c(-0.594, -1.5, 0)), "u")
  expect_equal(r$utility, c(-0.594, -1.5, 0))
})

test_that("values above 1 are reported but not discarded", {
  d <- data.frame(u = c(0.5, 1, 1.2, 3))
  expect_warning(r <- .prep_utility(d, "u"), "greater than 1")
  expect_warning(.prep_utility(d, "u"), "2 EQ-5D value")
  expect_equal(r$utility, c(0.5, 1, 1.2, 3))
})

test_that("unreadable values become NA, with a count", {
  d <- data.frame(u = c("0.5", "high", "1", "n/a"), stringsAsFactors = FALSE)
  expect_warning(r <- .prep_utility(d, "u"), "2 EQ-5D value")
  expect_equal(r$utility, c(0.5, NA, 1, NA))
  # Values already missing are not counted as coerced.
  expect_silent(.prep_utility(data.frame(u = c(0.5, NA)), "u"))
})

test_that("a factor is read through its labels, not its codes", {
  d <- data.frame(u = factor(c("0.8", "0.2", "0.8")))
  r <- .prep_utility(d, "u")
  # as.numeric() on the factor itself would give the level codes 2, 1, 2.
  expect_equal(r$utility, c(0.8, 0.2, 0.8))
})

test_that("name_utility defaults to \"utility\", with a message", {
  d <- data.frame(utility = c(1, 0.5), time = c("a", "b"),
                  stringsAsFactors = FALSE)
  expect_message(eq5d_utility_summary(d, name_fu = "time", levels_fu = c("a", "b")),
                 "Default column name will be used: utility")
  r <- q(eq5d_utility_summary(d, name_fu = "time", levels_fu = c("a", "b")))
  expect_equal(r[["a"]][r$name == "Mean"], 1)
})

test_that("the value analyses no longer take a value set", {
  # The whole point of the change: no names_eq5d, eq5d_version or country.
  fns <- c("eq5d_utility_summary", "eq5d_utility_summary_by_group",
           "eq5d_utility_norms_comparison", "eq5d_utility_over_time_plot",
           "eq5d_utility_by_group_plot", "eq5d_utility_change_by_group_plot",
           "eq5d_utility_distribution_plot", "eq5d_utility_vas_scatter_plot")
  for (fn in fns) {
    args <- names(formals(get(fn)))
    expect_true("name_utility" %in% args, info = fn)
    expect_false(any(c("names_eq5d", "eq5d_version", "country") %in% args),
                 info = fn)
  }
})

test_that("the values analysed are the ones supplied, not recalculated", {
  # Hand the function values that could not have come from any value set and
  # check they come back out: nothing recomputes them from the dimensions.
  d <- data.frame(mo = 1L, sc = 1L, ua = 1L, pd = 1L, ad = 1L,
                  value = c(0.25, 0.75), time = c("a", "b"),
                  stringsAsFactors = FALSE)
  r <- q(eq5d_utility_summary(d, name_utility = "value", name_fu = "time",
                              levels_fu = c("a", "b")))
  expect_equal(r[["a"]][r$name == "Mean"], 0.25)
  expect_equal(r[["b"]][r$name == "Mean"], 0.75)
  # The state is 11111, which every value set puts at 1.
  expect_false(isTRUE(all.equal(r[["a"]][r$name == "Mean"], 1)))
})

test_that("a missing value column is reported against its own argument", {
  msg <- tryCatch(
    q(eq5d_utility_distribution_plot(data.frame(mo = 1L), name_utility = "value")),
    error = function(e) conditionMessage(e))
  expect_match(msg, "\"value\" (from `name_utility`)", fixed = TRUE)
})
