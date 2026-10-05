# The value functions used to attach the input health state codes as names:
#
#   eq5d5l(c(11111, 22222), country = "GB")
#   #  11111  22222
#   #  1.000  0.577
#
# The names came from one line in eq5d(), and travelled to eq5d3l(), eq5d5l(),
# eq5dy3l() and eqxw(), which all delegate to it. They were undocumented, and
# they propagated into every data.frame column, plot label and comparison built
# on the result -- an identical() against a plain numeric vector failed on the
# names alone. Nothing in the package ever looked one up.
#
# Every value function now returns an unnamed numeric vector, in the order of
# the input.

x3 <- c(11111, 12321, 33333)
x5 <- c(11111, 12345, 55555)

# Every exported function that returns EQ-5D values, with a call that exercises
# it. Kept as a list so that a new one cannot quietly skip the check.
value_calls <- list(
  "eq5d(3L)"     = function() eq5d(x3, country = "GB", version = "3L"),
  "eq5d(5L)"     = function() eq5d(x5, country = "GB", version = "5L"),
  "eq5d(Y3L)"    = function() eq5d(x3, country = "NL", version = "Y3L"),
  "eq5d(XW)"     = function() eq5d(x5, country = "GB", version = "XW"),
  "eq5d(XWR)"    = function() eq5d(x3, country = "GB", version = "XWR"),
  "eq5d3l"       = function() eq5d3l(x3, country = "GB"),
  "eq5d5l"       = function() eq5d5l(x5, country = "GB"),
  "eq5dy3l"      = function() eq5dy3l(x3, country = "NL"),
  "eqxw"         = function() eqxw(x5, country = "GB"),
  "eqxwr"        = function() eqxwr(x3, country = "GB"),
  "eqxw_UK"      = function() eqxw_UK(x5, age = 50, male = 1),
  "eqxwr_UK"     = function() eqxwr_UK(x3, age = 50, male = 1),
  "eqxw_UK(val)" = function() eqxw_UK(c(0.5, 0.2), age = 50, male = 1,
                                      bwidth = 0.1),
  "eqxwr_UK(val)" = function() eqxwr_UK(c(0.5, 0.2), age = 50, male = 1,
                                        bwidth = 0.1))

quiet <- function(f) suppressWarnings(suppressMessages(f()))

# ---------------------------------------------------------------------------
# Unnamed
# ---------------------------------------------------------------------------

test_that("every value function returns an unnamed numeric vector", {
  for (nm in names(value_calls)) {
    r <- quiet(value_calls[[nm]])
    expect_null(names(r), info = nm)
    expect_type(r, "double")
    expect_null(dim(r), info = nm)
  }
})

test_that("the result compares equal to the same values written out plainly", {
  # This is what the names used to break: a comparison against a bare vector
  # failed on the names alone. (The EQ-5D-3L table is stored at float
  # precision, hence the tolerance; the 5L table is not.)
  expect_equal(eq5d3l(c(11111, 33333), country = "GB"), c(1, -0.594),
               tolerance = 1e-7)
  expect_identical(eq5d5l(11111, country = "GB"), 1)
  expect_identical(eq5d3l(11111, country = "GB"), 1)
})

# ---------------------------------------------------------------------------
# In the order of the input
# ---------------------------------------------------------------------------

test_that("values come back in the order they were asked for", {
  for (nm in names(value_calls)) {
    r <- quiet(value_calls[[nm]])
    expect_length(r, 3L - as.integer(grepl("(val)", nm, fixed = TRUE)))
  }

  # Reordering the input reorders the output, and nothing else.
  ord <- c(3L, 1L, 2L)
  expect_identical(eq5d3l(x3[ord], country = "GB"),
                   eq5d3l(x3, country = "GB")[ord])
  expect_identical(eq5d5l(x5[ord], country = "GB"),
                   eq5d5l(x5, country = "GB")[ord])
  expect_identical(suppressWarnings(eqxwr(x3[ord], country = "GB")),
                   suppressWarnings(eqxwr(x3, country = "GB"))[ord])
})

test_that("a repeated state does not collapse or reorder", {
  # Names would have been duplicated here; positions must simply repeat.
  r <- eq5d3l(c(11111, 33333, 11111), country = "GB")
  expect_null(names(r))
  expect_equal(r, c(1, -0.594, 1), tolerance = 1e-7)
  expect_identical(r[[1]], r[[3]])
})

test_that("invalid and missing states stay in place as NA", {
  r <- eq5d3l(c(11111, NA, 99999, 33333), country = "GB")
  expect_equal(r, c(1, NA, NA, -0.594), tolerance = 1e-7)
  expect_null(names(r))
})

# ---------------------------------------------------------------------------
# data.frame input, and several value sets at once
# ---------------------------------------------------------------------------

test_that("data.frame input also gives an unnamed vector", {
  d <- data.frame(mo = c(1, 3), sc = c(1, 3), ua = c(1, 3),
                  pd = c(1, 3), ad = c(1, 3))
  r <- eq5d3l(d, country = "GB")
  expect_null(names(r))
  expect_equal(r, c(1, -0.594), tolerance = 1e-7)
})

test_that("asking for several value sets keeps the value set names", {
  # Those name the columns, not the input, so they stay: one column per value
  # set is what the documentation promises. The rows have no names.
  r <- eq5d(x3, country = c("GB", "US"), version = "3L")
  expect_identical(colnames(r), c("GB", "US"))
  expect_null(rownames(r))
  expect_identical(nrow(r), 3L)
  expect_identical(r[, "GB"], eq5d3l(x3, country = "GB"))
})

# ---------------------------------------------------------------------------
# Downstream
# ---------------------------------------------------------------------------

test_that("the utility column added to a data frame carries no names", {
  d <- data.frame(mo = c(1L, 3L), sc = c(1L, 3L), ua = c(1L, 3L),
                  pd = c(1L, 3L), ad = c(1L, 3L))
  r <- .prep_eq5d(d, names = c("mo", "sc", "ua", "pd", "ad"),
                  add_state = TRUE, add_utility = TRUE,
                  eq5d_version = "3L", country = "GB")
  expect_null(names(r$utility))
  expect_equal(r$utility, c(1, -0.594), tolerance = 1e-7)
})

test_that("the Shiny helper's utility column carries no names", {
  d <- data.frame(mo = c(1L, 3L), sc = c(1L, 3L), ua = c(1L, 3L),
                  pd = c(1L, 3L), ad = c(1L, 3L))
  r <- compute_utility_col(d, method = "direct", country = "GB",
                           eq5d_version = "3L")
  expect_null(names(r))
})
