# The Health State Density Index is the one Zamora et al. (2018) define:
# HSDI = sum_i (x_i - x_{i-1}) (y_i + y_{i-1}), profiles ranked from most to
# least frequent, x the cumulative share of observations, y = i / S. It is 1
# for an even distribution and falls with concentration. The help page used to
# say the opposite (0 = even); the calculation was right and is unchanged.

DIMS <- c("mo", "sc", "ua", "pd", "ad")

# A data set in which the k-th of `freq` distinct 3L profiles is observed
# freq[k] times.
with_freq <- function(freq) {
  states <- make_all_EQ_states("3L")[seq_along(freq), DIMS]
  states[rep(seq_along(freq), freq), , drop = FALSE]
}

hsdi <- function(freq, version = "3L")
  suppressMessages(eq5d_profile_density_curve(
    with_freq(freq), names_eq5d = DIMS, eq5d_version = version))$hsdi

lower_bound <- function(N, S) 1 / S + (S - 1) / N

test_that("hand-computed values", {
  expect_equal(hsdi(c(1, 1)), 1)
  expect_equal(hsdi(c(5, 5)), 1)
  expect_equal(hsdi(c(9, 1)), 0.9 * 0.5 + 0.1 * 1.5)            # 0.6
  expect_equal(hsdi(c(9, 1)), 0.6)
  expect_equal(hsdi(c(999, 1)), 0.999 * 0.5 + 0.001 * 1.5)      # 0.501
  expect_equal(hsdi(c(2, 1, 1)), (0.5 * 1 + 0.25 * 3 + 0.25 * 5) / 3)
  expect_equal(hsdi(c(2, 1, 1)), 5 / 6)
  expect_equal(hsdi(c(3, 3, 3, 3)), 1)
})

test_that("a single profile is perfectly even", {
  expect_equal(hsdi(7), 1)
})

test_that("concentrating a fixed set of profiles lowers the index", {
  # Three profiles, twelve observations, increasingly concentrated.
  v <- c(hsdi(c(4, 4, 4)), hsdi(c(6, 3, 3)), hsdi(c(8, 2, 2)),
         hsdi(c(10, 1, 1)))
  expect_equal(v[1], 1)
  expect_true(all(diff(v) < 0))
  # The most concentrated reaches the attainable lower bound.
  expect_equal(v[4], lower_bound(12, 3))
})

test_that("the index lies between 1/S + (S - 1)/N and 1", {
  withr::with_seed(42, {
    for (k in 1:50) {
      S <- sample(2:12, 1)
      freq <- sample(1:30, S, replace = TRUE)
      h <- hsdi(freq)
      expect_gte(h, lower_bound(sum(freq), S) - 1e-12)
      expect_lte(h, 1 + 1e-12)
      expect_gt(h, 1 / S)
    }
  })
  # Two profiles never go below 0.5.
  expect_gt(hsdi(c(1e4, 1)), 0.5)
})

test_that("repeating a distribution over more profiles leaves it unchanged", {
  expect_equal(hsdi(c(5, 2, 1, 1, 5, 2, 1, 1)), hsdi(c(5, 2, 1, 1)))
  expect_equal(hsdi(rep(c(1479, 1, 1, 1), 3)), hsdi(c(1479, 1, 1, 1)))
})

test_that("the order of equally frequent profiles does not matter", {
  expect_equal(hsdi(c(4, 2, 2, 1)), hsdi(c(4, 2, 1, 2)))
  expect_equal(hsdi(c(1, 2, 4, 2)), hsdi(c(4, 2, 2, 1)))
})

test_that("only observed profiles count, whatever the instrument allows", {
  # The same observations analysed as 3L and as 5L data: 243 versus 3,125
  # possible profiles, the same three observed, the same index.
  expect_equal(hsdi(c(6, 3, 3), "5L"), hsdi(c(6, 3, 3), "3L"))
  r <- suppressMessages(eq5d_profile_density_curve(
    with_freq(c(6, 3, 3)), names_eq5d = DIMS, eq5d_version = "3L"))
  expect_equal(r$plot_data$CumPropStates, (1:3) / 3)
  expect_equal(r$plot_data$CumPropObservations, c(6, 9, 12) / 12)
})

test_that("an invalid profile is excluded, with a warning", {
  d <- with_freq(c(9, 1))
  d$mo[1] <- 4                     # not a 3L level
  w <- testthat::capture_warnings(
    r <- suppressMessages(eq5d_profile_density_curve(d, names_eq5d = DIMS,
                                                     eq5d_version = "3L")))
  expect_true(any(grepl("excluded", w)))
  # 8 : 1 over N = 9.
  expect_equal(r$hsdi, (8 / 9) * 0.5 + (1 / 9) * 1.5)
})
