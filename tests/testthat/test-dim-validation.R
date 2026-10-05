# Dimension values are validated before they are encoded.
#
# Review findings F01 and F17. Before this, every path coerced with
# as.integer() and range-checked the result, so a fractional level was
# truncated into a valid one and an out-of-range level could carry into the
# dimension beside it. The scorers, the analysis functions and the app's
# validation page now share one predicate: .dim_status() in R/validate_dims.R.

dims <- c("mo", "sc", "ua", "pd", "ad")
row <- function(...) {
  v <- c(...)
  as.data.frame(as.list(stats::setNames(v, dims)))
}
q <- function(expr) suppressWarnings(suppressMessages(expr))

# ---------------------------------------------------------------------------
# F01: invalid values cannot become valid states
# ---------------------------------------------------------------------------

test_that("an out-of-range level cannot carry into the dimension beside it", {
  # c(0, 11, 1, 1, 1) used to encode as 11111 -- 0 * 10000 contributes
  # nothing and 11 * 1000 lands in the first two digits -- and then scored as
  # full health, which is the worst possible answer for a malformed record.
  expect_true(is.na(q(toEQ5Dindex(row(0, 11, 1, 1, 1)))))
  expect_true(is.na(q(eq5d3l(row(0, 11, 1, 1, 1), "GB"))))
  expect_warning(toEQ5Dindex(row(0, 11, 1, 1, 1)),
                 "not a level the instrument allows")
})

test_that("fractional levels are rejected rather than truncated", {
  # 1.9 became level 1 and 2.9 became level 2, each scored as a real state.
  for (v in c(1.9, 2.9, 1.0001)) {
    expect_true(is.na(q(toEQ5Dindex(row(v, 1, 1, 1, 1)))), info = v)
    expect_true(is.na(q(eq5d3l(row(v, 1, 1, 1, 1), "GB"))), info = v)
  }
  expect_warning(toEQ5Dindex(row(1.9, 1, 1, 1, 1)), "not rounded")
  # A whole number stored as a double is not fractional.
  expect_equal(q(toEQ5Dindex(row(2, 1, 1, 1, 1))), 21111L)
})

test_that("non-finite levels are rejected", {
  expect_true(is.na(q(toEQ5Dindex(row(Inf, 1, 1, 1, 1)))))
  expect_true(is.na(q(toEQ5Dindex(row(-Inf, 1, 1, 1, 1)))))
  expect_warning(toEQ5Dindex(row(Inf, 1, 1, 1, 1)), "not finite")
  # NaN and NA are ordinary missingness, not a reportable problem.
  expect_true(is.na(q(toEQ5Dindex(row(NaN, 1, 1, 1, 1)))))
  expect_silent(toEQ5Dindex(row(NA, 1, 1, 1, 1), quiet = TRUE))
})

test_that("valid states are untouched and keep their order", {
  d <- q(make_all_EQ_states("3L"))
  expect_silent(idx <- toEQ5Dindex(d, quiet = TRUE))
  expect_identical(idx, q(make_all_EQ_indexes("3L")))
  # And valid rows beside an invalid one are unaffected.
  mixed <- rbind(row(1, 1, 1, 1, 1), row(1.9, 1, 1, 1, 1), row(3, 2, 1, 2, 1))
  got <- q(toEQ5Dindex(mixed))
  expect_identical(got, c(11111L, NA_integer_, 32121L))
  expect_equal(q(eq5d3l(mixed, "GB"))[c(1L, 3L)],
               q(eq5d3l(rbind(row(1, 1, 1, 1, 1), row(3, 2, 1, 2, 1)), "GB")))
})

test_that("a named vector and a one-row frame agree", {
  for (v in list(c(1, 1, 1, 1, 1), c(1.9, 1, 1, 1, 1), c(0, 11, 1, 1, 1))) {
    nv <- stats::setNames(v, dims)
    expect_identical(q(toEQ5Dindex(nv)), q(toEQ5Dindex(row(v[1], v[2], v[3], v[4], v[5]))),
                     info = paste(v, collapse = ","))
  }
})

test_that("factor dimension columns are read by label, not level code", {
  # A column whose only observed label is "3" has level code 1.
  f <- as.data.frame(lapply(stats::setNames(rep("3", 5), dims), factor))
  expect_equal(q(toEQ5Dindex(f)), 33333L)
  expect_equal(q(eq5d3l(f, "GB")), q(eq5d3l(row(3, 3, 3, 3, 3), "GB")))
})

test_that("the version decides the range in the analysis path", {
  # 4 and 5 are levels for the EQ-5D-5L and not for the 3L or Y-3L.
  d <- row(5, 4, 1, 1, 1)
  got5 <- q(.prep_eq5d(d, names = dims, eq5d_version = "5L", add_state = TRUE))
  expect_identical(got5$state, "54111")
  got3 <- q(.prep_eq5d(d, names = dims, eq5d_version = "3L", add_state = TRUE))
  expect_true(is.na(got3$state))
})

test_that("na.rm = TRUE still substitutes zero, as documented", {
  # The documented escape hatch produces a code with a zero digit on purpose;
  # the new digit bound must not break it.
  expect_equal(q(toEQ5Dindex(row(NA, 1, 1, 1, 1), na.rm = TRUE)), 1111L)
})

# ---------------------------------------------------------------------------
# F01: eq5d_validate() agrees with the analysis
# ---------------------------------------------------------------------------

test_that("eq5d_validate() flags what the analysis will discard", {
  m <- list(eq5d_version = "3L", names_eq5d = dims)
  flagged <- function(d) {
    v <- q(eq5d_validate(d, m, quiet = TRUE))
    any(grepl("not a level the instrument allows", v$message))
  }
  # Each of these used to be reported as "All EQ-5D values within expected
  # range" and then silently analysed, or discarded without warning.
  expect_true(flagged(row(1.9, 1, 1, 1, 1)))
  expect_true(flagged(row(Inf, 1, 1, 1, 1)))
  expect_true(flagged(row(0, 11, 1, 1, 1)))
  expect_true(flagged(row(4, 1, 1, 1, 1)))
  expect_false(flagged(row(1, 2, 3, 1, 1)))
  # And a rejected value counts as missing, not as present.
  v <- q(eq5d_validate(row(1.9, 1, 1, 1, 1), m, quiet = TRUE))
  expect_true(any(grepl("have missing EQ-5D values", v$message)))
})

# ---------------------------------------------------------------------------
# F17: make_dummies() accepts the matrix input it documents
# ---------------------------------------------------------------------------

test_that("make_dummies() gives the same answer for a matrix and a frame", {
  st <- q(make_all_EQ_states("3L"))
  for (n in c(1L, 2L, 10L)) {
    d <- st[seq_len(n), , drop = FALSE]
    expect_equal(q(make_dummies(as.matrix(d), "3L")), q(make_dummies(d, "3L")),
                 info = paste(n, "rows"))
  }
})

test_that("make_dummies() accepts a matrix under every option", {
  m <- as.matrix(q(make_all_EQ_states("5L"))[1:3, , drop = FALSE])
  d <- q(make_all_EQ_states("5L"))[1:3, , drop = FALSE]
  for (opt in list(list(incremental = TRUE), list(add_intercept = TRUE),
                   list(drop_level_1 = FALSE), list(return_df = FALSE),
                   list(incremental = TRUE, add_intercept = TRUE))) {
    a <- do.call(make_dummies, c(list(m, "5L"), opt))
    b <- do.call(make_dummies, c(list(d, "5L"), opt))
    expect_equal(q(a), q(b), info = paste(names(opt), collapse = "+"))
  }
})

# ---------------------------------------------------------------------------
# Every public entry point, and the instrument-specific range
# ---------------------------------------------------------------------------

# Level 4 is a level for the EQ-5D-5L and for nothing else, so it is the value
# that tells the instruments apart. Level 6 is a level for none of them.
#
# Note the two kinds of protection, which differ in their diagnostics but not
# in their result. A value that is fractional, non-finite or not a single
# digit is rejected when the state is encoded, with a warning. A value that is
# a digit but not a level of this instrument -- 4 for a three-level
# instrument -- produces a state code that is not in the instrument's value
# set, and the lookup returns NA silently. Neither can yield a wrong number.
entry_points <- list(
  "eq5d3l"         = function(d) eq5d3l(d, "GB"),
  "eq5d5l"         = function(d) eq5d5l(d, "GB"),
  "eq5dy3l"        = function(d) eq5dy3l(d, "SI"),
  "eq5d 3L"        = function(d) eq5d(d, country = "GB", version = "3L"),
  "eq5d 5L"        = function(d) eq5d(d, country = "GB", version = "5L"),
  "eq5d Y3L"       = function(d) eq5d(d, country = "SI", version = "Y3L"),
  "eqxw 5L->3L"    = function(d) eqxw(d, "GB"),
  "eqxwr 3L->5L"   = function(d) eqxwr(d, "GB"),
  "eqxw_UK 5L"     = function(d) eqxw_UK(d, age = 30, male = 1),
  "eqxwr_UK 3L"    = function(d) eqxwr_UK(d, age = 30, male = 1)
)
# Which entry points take five-level input.
five_level <- c("eq5d5l", "eq5d 5L", "eqxw 5L->3L", "eqxw_UK 5L")

test_that("no entry point turns an invalid level into a number", {
  for (nm in names(entry_points)) {
    f <- entry_points[[nm]]
    expect_false(is.na(q(f(row(1, 1, 1, 1, 1)))), info = paste(nm, "valid"))
    # 4 is accepted only by the five-level instruments.
    got4 <- q(f(row(4, 1, 1, 1, 1)))
    if (nm %in% five_level) expect_false(is.na(got4), info = paste(nm, "L4"))
    else expect_true(is.na(got4), info = paste(nm, "L4"))
    # And these are invalid everywhere.
    for (bad in list(6, 0, 1.9, -1, Inf))
      expect_true(is.na(q(f(row(bad, 1, 1, 1, 1)))),
                  info = paste(nm, "level", bad))
  }
})

test_that("an invalid row does not disturb the rows around it", {
  mixed <- rbind(row(1, 1, 1, 1, 1), row(1.9, 1, 1, 1, 1), row(3, 2, 1, 2, 1),
                 row(0, 11, 1, 1, 1), row(2, 2, 2, 2, 2))
  clean <- rbind(row(1, 1, 1, 1, 1), row(3, 2, 1, 2, 1), row(2, 2, 2, 2, 2))
  keep <- c(1L, 3L, 5L)
  for (nm in c("eq5d3l", "eqxwr 3L->5L", "eqxwr_UK 3L")) {
    f <- entry_points[[nm]]
    got <- q(f(mixed))
    expect_length(got, 5L)
    expect_true(all(is.na(got[c(2L, 4L)])), info = nm)
    expect_equal(got[keep], q(f(clean)), info = nm)
  }
})

test_that("all three instruments reject their own out-of-range levels", {
  # The boundary is the highest level each instrument allows.
  for (spec in list(list(v = "3L", c = "GB", hi = 3L),
                    list(v = "5L", c = "GB", hi = 5L),
                    list(v = "Y3L", c = "SI", hi = 3L))) {
    at <- row(spec$hi, 1, 1, 1, 1)
    over <- row(spec$hi + 1L, 1, 1, 1, 1)
    expect_false(is.na(q(eq5d(at, country = spec$c, version = spec$v))),
                 info = paste(spec$v, "at the boundary"))
    expect_true(is.na(q(eq5d(over, country = spec$c, version = spec$v))),
                info = paste(spec$v, "over the boundary"))
    # .prep_eq5d() rejects it with a warning, where the version is known.
    got <- q(.prep_eq5d(over, names = dims, eq5d_version = spec$v,
                        add_state = TRUE))
    expect_true(is.na(got$state), info = spec$v)
  }
})

# ---------------------------------------------------------------------------
# na.rm = TRUE: documented, and unable to manufacture a plausible state
# ---------------------------------------------------------------------------

test_that("na.rm = TRUE substitutes zero for missing and for invalid alike", {
  # Documented behaviour: NA becomes 0, which changes the index. A rejected
  # value is NA by the time the substitution happens, so it does too.
  expect_equal(q(toEQ5Dindex(row(NA, 1, 1, 1, 1), na.rm = TRUE)), 1111L)
  expect_equal(q(toEQ5Dindex(row(11, 1, 1, 1, 1), na.rm = TRUE)), 1111L)
  expect_equal(q(toEQ5Dindex(row(1.9, 1, 1, 1, 1), na.rm = TRUE)), 1111L)
  expect_equal(q(toEQ5Dindex(row(NA, 11, 1, 1, 1), na.rm = TRUE)), 111L)
  expect_equal(q(toEQ5Dindex(row(NA, NA, NA, NA, NA), na.rm = TRUE)), 0L)
})

test_that("na.rm = TRUE cannot make invalid data into a scored state", {
  # A zero digit can never appear in a valid state code, so every index
  # na.rm = TRUE produces from an invalid value falls outside both value sets
  # and scores NA. Checked over every bad value in every position.
  states <- c(q(make_all_EQ_indexes("3L")), q(make_all_EQ_indexes("5L")))
  for (bad in list(0, 11, 1.9, -1, 99, Inf, NA)) {
    for (pos in 1:5) {
      v <- rep(1, 5); v[pos] <- bad[[1L]]
      d <- row(v[1], v[2], v[3], v[4], v[5])
      idx <- q(toEQ5Dindex(d, na.rm = TRUE))
      expect_false(!is.na(idx) && idx %in% states,
                   info = paste("value", bad, "in position", pos))
      expect_true(is.na(q(eq5d3l(d, "GB"))), info = paste(bad, pos))
      expect_true(is.na(q(eq5d5l(d, "GB"))), info = paste(bad, pos))
    }
  }
})
