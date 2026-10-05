# Regression baseline for eqxw_UK() with health state input.
#
# The fixture holds the mapped EQ-5D-3L value (UK, Dolan 1997) for every
# combination of
#   * all 3,125 EQ-5D-5L states,
#   * ten ages covering both ends of each NICE age band
#     (18/34, 35/44, 45/54, 55/64, 65/99), and
#   * both sexes,
# i.e. 62,500 rows.
#
# The values come from .crosswalk_NICE, which was verified against the NICE
# DSU's EQ5Dmap_table5.csv (_EUUKcopula, April 2026 release) to a maximum
# absolute difference of 9.99e-16 across all 31,250 table rows. They must not
# change. The fixture is the .rds beside this file; regenerating it is a
# deliberate act, and would mean evaluating eqxw_UK() over the grid above.
#
# Age 100 is deliberately excluded: the current cut() banding returns NA at
# exactly 100, and the age handling is being replaced. Age 100 and the other
# boundary cases are covered by the edge-case tests instead.

fixture_path <- test_path("fixture-eqxw-uk-baseline.rds")

test_that("baseline fixture is intact", {
  fx <- readRDS(fixture_path)
  expect_s3_class(fx, "data.frame")
  expect_named(fx, c("state", "age", "male", "value"))
  expect_equal(nrow(fx), 62500L)
  expect_equal(length(unique(fx$state)), 3125L)
  expect_equal(sort(unique(fx$age)), c(18, 34, 35, 44, 45, 54, 55, 64, 65, 99))
  expect_equal(sort(unique(fx$male)), c(0, 1))
  expect_false(anyNA(fx$value))
})

test_that("eqxw_UK() reproduces the DSU-verified mapping exactly", {
  fx  <- readRDS(fixture_path)
  got <- as.numeric(eqxw_UK(fx$state, age = fx$age, male = fx$male))

  expect_equal(length(got), nrow(fx))
  expect_false(anyNA(got))

  # Order-independent: compares the multiset of mapped values. This holds both
  # before and after the input-order fix, so it is the guard that must never
  # break.
  #
  # Tolerance is 1e-12, not 0. The fixture was taken from .crosswalk_NICE as it
  # stood before the tables were re-imported from the DSU CSVs; re-reading the
  # same numbers from text and back to double moves them by at most 9.99e-16.
  # That is float round-trip, not a change in the mapping: it is 12 orders of
  # magnitude below the smallest meaningful difference in an EQ-5D value.
  expect_equal(sort(got), sort(fx$value), tolerance = 1e-12)
})

test_that("eqxw_UK() returns results in input order", {
  # Before the rewrite this failed: the function built its result with
  # merge(..., sort = FALSE), which reorders, so only 20 of the 62,500 values
  # landed in the correct position even though the multiset was correct. The
  # lookup is now an explicit match() against the input, so order is guaranteed.
  fx  <- readRDS(fixture_path)
  got <- as.numeric(eqxw_UK(fx$state, age = fx$age, male = fx$male))
  expect_equal(got, fx$value, tolerance = 1e-12)
})
