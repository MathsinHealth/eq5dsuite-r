# Edge cases for eqxw_UK() and eqxwr_UK().
#
# These cover the input handling shared by both functions via .eqxw_NICE():
# invalid levels, missing values, the age boundaries, invalid sex codings, each
# accepted input type, and multi-row calls with the DSU's recommended
# bandwidths. The numeric agreement with the DSU is checked separately in
# test-dsu-validation.R.

# ---- health states ----------------------------------------------------------

test_that("invalid health states return NA", {
  # level 6 does not exist on either instrument
  expect_true(is.na(suppressWarnings(eqxw_UK(11116, age = 30, male = 1))))
  # level 4 and 5 do not exist on the EQ-5D-3L
  expect_true(is.na(suppressWarnings(eqxwr_UK(11114, age = 30, male = 1))))
  expect_true(is.na(suppressWarnings(eqxwr_UK(55555, age = 30, male = 1))))
  # too few digits
  expect_true(is.na(suppressWarnings(eqxw_UK(1111, age = 30, male = 1))))
})

test_that("a 3L state is valid input to eqxwr_UK() and maps to a 5L value", {
  v <- eqxwr_UK(33333, age = 30, male = 1)
  expect_false(is.na(v))
  expect_true(v >= -1 && v <= 1)
})

test_that("missing values propagate as NA without affecting their neighbours", {
  got <- suppressWarnings(
    eqxw_UK(c(11111, NA, 55555), age = c(30, 30, 30), male = c(1, 1, 1)))
  expect_equal(length(got), 3L)
  expect_true(is.na(got[2]))
  expect_false(anyNA(got[c(1, 3)]))

  got <- suppressWarnings(
    eqxw_UK(c(11111, 11111, 11111), age = c(30, NA, 30), male = c(1, 1, NA)))
  expect_true(all(is.na(got[2:3])))
  expect_false(is.na(got[1]))
})

# ---- age --------------------------------------------------------------------

test_that("ages below 16 return NA with a warning", {
  expect_warning(v <- eqxw_UK(11111, age = 15, male = 1), "below 16")
  expect_true(is.na(v))
  expect_warning(v <- eqxw_UK(11111, age = 0, male = 1), "below 16")
  expect_true(is.na(v))
})

test_that("ages 1 to 5 are ages, never band numbers", {
  # The DSU's own R command reads 1-5 as a band index; this must not.
  expect_warning(v <- eqxw_UK(11111, age = 5, male = 1), "below 16")
  expect_true(is.na(v))
})

test_that("age 16 is the first valid age and falls in band 1", {
  expect_silent(v16 <- eqxw_UK(11111, age = 16, male = 1))
  expect_false(is.na(v16))
  expect_equal(v16, eqxw_UK(11111, age = 34, male = 1))   # same band
})

test_that("each band boundary lands in the intended band", {
  # Ages that share a band must give identical values; ages either side must not.
  bands <- list(c(16, 34), c(35, 44), c(45, 54), c(55, 64), c(65, 99))
  vals <- vapply(bands, function(b) {
    v <- eqxw_UK(rep(12345, 2), age = b, male = c(1, 1))
    expect_equal(v[1], v[2])                                # same band
    v[1]
  }, numeric(1))
  expect_equal(length(unique(vals)), 5L)                    # five distinct bands
})

test_that("there is no upper age limit", {
  v100 <- eqxw_UK(11111, age = 100, male = 1)
  expect_false(is.na(v100))
  expect_equal(v100, eqxw_UK(11111, age = 65, male = 1))    # band 5
  expect_equal(v100, eqxw_UK(11111, age = 120, male = 1))
  expect_equal(v100, eqxw_UK(11111, age = 999, male = 1))
})

# ---- sex --------------------------------------------------------------------

test_that("male accepts 1/0 and TRUE/FALSE", {
  expect_equal(eqxw_UK(12345, age = 40, male = TRUE),  eqxw_UK(12345, age = 40, male = 1))
  expect_equal(eqxw_UK(12345, age = 40, male = FALSE), eqxw_UK(12345, age = 40, male = 0))
  expect_false(isTRUE(all.equal(eqxw_UK(12345, age = 40, male = 1),
                                eqxw_UK(12345, age = 40, male = 0))))
})

test_that("other sex values warn and return NA", {
  expect_warning(v <- eqxw_UK(11111, age = 30, male = 2), "neither 0 nor 1")
  expect_true(is.na(v))
  expect_warning(v <- eqxw_UK(11111, age = 30, male = -1), "neither 0 nor 1")
  expect_true(is.na(v))
  expect_warning(v <- eqxw_UK(11111, age = 30, male = "m"), "neither 0 nor 1")
  expect_true(is.na(v))
})

# ---- input types ------------------------------------------------------------

test_that("vector, matrix and data frame inputs agree", {
  target <- eqxw_UK(12345, age = 30, male = 1)

  m <- matrix(c(1, 2, 3, 4, 5), nrow = 1,
              dimnames = list(NULL, c("mo", "sc", "ua", "pd", "ad")))
  expect_equal(eqxw_UK(m, age = 30, male = 1), target)

  df <- data.frame(mo = 1, sc = 2, ua = 3, pd = 4, ad = 5, age = 30, male = 1)
  expect_equal(eqxw_UK(df, age = "age", male = "male"), target)   # column names
  expect_equal(eqxw_UK(df, age = 30, male = 1), target)           # vectors
})

test_that("results follow the order of the input, including repeated states", {
  # Repeated (state, age band, sex) keys are what the pre-2.1.0 merge-based
  # lookup got wrong: values were grouped by key rather than left in input
  # order. A B A B must come back as A B A B.
  s <- c(11111, 22222, 11111, 22222)
  got <- eqxw_UK(s, age = rep(30, 4), male = rep(1, 4))
  expect_equal(got[1], got[3])
  expect_equal(got[2], got[4])
  expect_false(isTRUE(all.equal(got[1], got[2])))

  # a repeat late in a longer vector must not shift everything after it
  s2 <- c(11111, 22222, 33333, 11111)
  g2 <- eqxw_UK(s2, age = rep(30, 4), male = rep(1, 4))
  expect_equal(g2[1], g2[4])
  expect_equal(g2[1:3], eqxw_UK(c(11111, 22222, 33333), age = rep(30, 3), male = rep(1, 3)))
})

test_that("age and male recycle from length 1, and mismatched lengths error", {
  expect_equal(length(eqxw_UK(c(11111, 22222, 33333), age = 30, male = 1)), 3L)
  expect_error(eqxw_UK(c(11111, 22222, 33333), age = c(30, 40), male = 1),
               "length 1 or 3")
})

# ---- value (score) input ----------------------------------------------------

test_that("bwidth = \"default\" is vectorised over rows", {
  # The DSU's own implementation fails here with "the condition has length > 1".
  got <- eqxwr_UK(c(0.95, 0.54), age = c(70, 35), male = c(1, 0), bwidth = "default")
  expect_equal(length(got), 2L)
  expect_false(anyNA(got))

  # 0.95 is above the 0.6 threshold, so it uses bandwidth 0.1; 0.54 uses 0.4.
  expect_equal(got[1], eqxwr_UK(0.95, age = 70, male = 1, bwidth = 0.1))
  expect_equal(got[2], eqxwr_UK(0.54, age = 35, male = 0, bwidth = 0.4))
})

test_that("the recommended bandwidth switches at 0.6 inclusive", {
  at    <- eqxwr_UK(0.6,  age = 40, male = 1, bwidth = "default")
  above <- eqxwr_UK(0.61, age = 40, male = 1, bwidth = "default")
  expect_equal(at,    eqxwr_UK(0.6,  age = 40, male = 1, bwidth = 0.4))
  expect_equal(above, eqxwr_UK(0.61, age = 40, male = 1, bwidth = 0.1))
})

test_that("bwidth = 0 behaves as the DSU's 1e-6", {
  expect_equal(eqxwr_UK(0.692, age = 40, male = 1, bwidth = 0),
               eqxwr_UK(0.692, age = 40, male = 1, bwidth = 1e-6))
})

test_that("a value with nothing inside the bandwidth returns NA with a message", {
  # -5 is far outside the range of either value set.
  expect_message(v <- eqxwr_UK(-5, age = 40, male = 1, bwidth = 0.01),
                 "within the bandwidth")
  expect_true(is.na(v))
})

test_that("an unsupported bwidth string is an error", {
  expect_error(eqxwr_UK(0.5, age = 40, male = 1, bwidth = "wide"),
               "numeric or")
})
